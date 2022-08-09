package parser

import asts.*
import lexer.{Lexer, Token}

import scala.annotation.tailrec
import scala.language.postfixOps

/**
 * 解析器
 */
object Parser {
  // 分词器
  var lexer: Lexer = new Lexer()

  /**
   * 解析名称
   *
   * @return 解析结果
   */
  def parseName(): Option[String] =
    lexer.FindAndMoveToNextTokenWithType(Token.NAME) match {
      case Some((_, name)) => Some(name)
      case _ => None
    }

  /**
   * 解析字符串
   *
   * @return 解析结果
   */
  def parseString(): Option[String] = {
    lexer.LookAhead() match {
      case Some(Token.DUOQUOTE) =>
        // 查找并移动到双引号后面
        lexer.FindAndMoveToNextTokenWithType(Token.DUOQUOTE)
        // 令标志位跳过所有空白字符
        lexer.LookAheadAndSkip(Token.IGNORED)
        // 返回解析结果
        Some("")
      case Some(Token.QUOTE) =>
        // 查找并移动到第一个引号后边
        lexer.FindAndMoveToNextTokenWithType(Token.QUOTE)
        // 查找，直到下一个引号
        lexer.scanUntilToken("\"") match {
          case Some(str) =>
            // 将标志位移动到后一个引号后边
            lexer.FindAndMoveToNextTokenWithType(Token.QUOTE)
            // 跳过所有空白字符
            lexer.LookAheadAndSkip(Token.IGNORED)
            // 返回解析结果
            Some(str)
          case None =>
            None
        }
      case _ =>
        println("parseString(): not a string.")
        None
    }
  }

  /**
   * 解析变量
   *
   * @return 解析结果
   */
  def parseVariable(): Option[Variable] = {
    // 获取行号，行号是用来提示错误信息的
    val lineNum = lexer.getLineNum
    // 解析变量符号
    lexer.FindAndMoveToNextTokenWithType(Token.VAR_PREFIX) match {
      case Some(_) =>
        // 变量符号后应该接变量名
        parseName() match {
          case Some(name) =>
            // 跳过变量后面的空白内容
            lexer.LookAheadAndSkip(Token.IGNORED)
            // 返回解析结果
            Some(Variable(lineNum, name))
          case _ =>
            // 如果只有变量符号，输出提示
            printf("parseVariable(): expect a variable name at line %n", lineNum)
            None
        }
      case None =>
        printf("parseVariable(): expect a \"$\" at line %n", lineNum)
        None
    }
  }

  /**
   * 解析赋值
   *
   * @return 解析结果
   */
  def parseAssignment(): Option[Assignment] = {
    val lineNum = lexer.getLineNum
    parseVariable() match {
      case Some(variable) =>
        // 找到变量后，跳过空白字符
        lexer.LookAheadAndSkip(Token.IGNORED)
        // 寻找赋值符号
        lexer.FindAndMoveToNextTokenWithType(Token.EQUAL) match {
          case Some(_) =>
            // 找到赋值符号后，跳过空白字符
            lexer.LookAheadAndSkip(Token.IGNORED)
            // 查找可赋值的值（此处只支持字符串）
            parseString() match {
              case Some(str) =>
                // 跳过空白部分
                lexer.LookAheadAndSkip(Token.IGNORED)
                // 返回赋值 AST
                Some(Assignment(lineNum, variable, str))
              case _ => None
            }
          case None => None
        }
      case None => None
    }
  }

  /**
   * 解析输出语句
   *
   * @return 解析结果
   */
  def parsePrint(): Option[Print] = {
    val lineNum = lexer.getLineNum
    // 寻找输出符号
    lexer.FindAndMoveToNextTokenWithType(Token.PRINT) match {
      case Some(_) =>
        // 寻找左括号
        lexer.FindAndMoveToNextTokenWithType(Token.LEFT_PAREN) match {
          case Some(_) =>
            // 跳过空白部分
            lexer.LookAheadAndSkip(Token.IGNORED)
            // 寻找变量
            parseVariable() match {
              case Some(variable) =>
                // 跳过空白部分
                lexer.LookAheadAndSkip(Token.IGNORED)
                // 寻找右括号
                lexer.FindAndMoveToNextTokenWithType(Token.RIGHT_PAREN) match {
                  case Some(_) =>
                    // 跳过空白部分
                    lexer.LookAheadAndSkip(Token.IGNORED)
                    // 返回输出结果
                    Some(Print(lineNum, variable))
                  case None => None
                }
              case None => None
            }
          case None => None
        }
      case None => None
    }
  }

  /**
   * 解析语句（组装方法）
   *
   * @return 解析结果
   */
  def parseStatement(): Option[Statement] = {
    // 根据下一个 Token 判断解析目标（这句话像……）
    lexer.LookAhead() match {
      case Some(Token.PRINT) => parsePrint()
      case Some(Token.VAR_PREFIX) => parseAssignment()
      case _ => printf("parseStatement(): unknown Ast.Statement."); None
    }
  }

  /**
   * 循环解析语句
   *
   * @return 解析结果
   */
  def parseStatements(): Option[List[Statement]] = {
    /**
     * 内部方法，循环解析语句
     *
     * @param statements 已经扫到的语句
     * @return 解析结果
     */
    @tailrec def innerScan(statements: List[Statement]): Option[List[Statement]] = {
      lexer.LookAhead() match {
        // 如果是 EOF 就结束扫描
        case Some(Token.EOF) => Some(statements)
        // 如果存在内容就尝试解析语句
        case Some(_) => parseStatement() match {
          case Some(stmt) =>
            // 如果解析到了语句，添加到列表并继续解析
            innerScan(statements ::: List(stmt))
          case None => None
        }
        case None => None
      }
    }

    innerScan(Nil)
  }

  /**
   * 解析源码
   *
   * @return 解析结果
   */
  def parseSourceCode(): Option[SourceCode] = {
    val lineNum = lexer.getLineNum
    parseStatements() match {
      case Some(stmts) => Some(SourceCode(lineNum, stmts))
      case None => None
    }
  }

  /**
   * 解析代码
   *
   * @param code 源码
   * @return 解析结果
   */
  def parse(code: String): Option[SourceCode] = {
    lexer.remainSourceCode = code
    parseSourceCode() match {
      case Some(sc) =>
        // 期望下一个 token 是 EOF，如果不是就报错
        lexer.FindAndMoveToNextTokenWithType(Token.EOF)
        Some(sc)
      case None => None
    }
  }

}
