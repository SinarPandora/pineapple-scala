package lexer

import lexer.Token.Token

import java.lang.Character.isLetter
import scala.annotation.tailrec
import scala.util.matching.Regex

/**
 * 分词器
 */
class Lexer {
  // 当前源码
  var remainSourceCode: String = ""
  // 下一个 Token
  private var nextToken: String = ""
  // 当前行数
  private var lineNum: Int = 1
  // 下一个 Token 行数
  private var nextTokenLineNum: Int = 0
  // 下一个 Token 类型
  private var nextTokenType: Token = Token.EOF
  // 提取名称的正则
  private val namePattern: Regex = raw"^([_\d\w]+)".r

  /**
   * 获取行号
   *
   * @return 行号
   */
  def getLineNum: Int = lineNum

  /**
   * 跳过一定位数的源码
   *
   * @param skip 跳过位数
   */
  def skipSourceCodeChars(skip: Int): Unit = {
    remainSourceCode = remainSourceCode.substring(skip)
  }

  /**
   * 扫描名称
   *
   * @return 名称 -> Token
   */
  def scanName(): (String, Option[Token]) = {
    namePattern.findFirstIn(remainSourceCode) match {
      case Some(name) =>
        // 扫到了，提取，并让下次扫描跳过这个名称
        skipSourceCodeChars(name.length())
        if (name == "print") {
          // Print 是一个特殊的名称
          (name, Some(Token.PRINT))
        } else {
          (name, None)
        }
      case _ => ("", None) // 没扫到名称
    }
  }

  /**
   * 判断下一部分源码是否以目标字符串开头
   *
   * @param startWith 目标字符串
   * @return 判断结果
   */
  def nextSourceCodeIs(startWith: String): Boolean = remainSourceCode.startsWith(startWith)

  /**
   * 判断下面是否是有意义的源码
   *
   * @return 判断结果
   */
  def nextIsNotSource: Boolean = {
    /**
     * 源码扫描
     *
     * @param notFound 未找到源码（继续扫描）
     * @param skip     跳过（位数）
     * @return 判断结果
     */
    @tailrec def innerScan(notFound: Boolean, skip: Int): Boolean = {
      if (skip >= remainSourceCode.length) {
        // 如果跳过了所有的源码
        // 移动扫描标志位
        skipSourceCodeChars(skip)
        // 返回 true
        true
      } else {
        // 跳过空白字符并继续扫描
        if (nextSourceCodeIs("\r\n") || nextSourceCodeIs("\n\r")) {
          lineNum += 1 // 遇到 windows 换行时直接前进一行
          innerScan(notFound = true, skip + 2)
        } else {
          remainSourceCode(skip) match {
            case '\n' | '\r' =>
              lineNum += 1 // 遇到 *nix 换行时直接前进一行
              innerScan(notFound = true, skip + 1)
            case '\t' | '\f' | ' ' =>
              innerScan(notFound = true, skip + 1) // 遇到空白字符时直接前进一行
            case _ =>
              // 发现有意义字符时
              // 跳过无意义字符
              skipSourceCodeChars(skip)
              // 返回 false
              false
          }
        }
      }
    }

    innerScan(notFound = false, 0)
  }

  /**
   * 识别 Token
   *
   * @return 行数，类型，值
   */
  def MatchToken(): Option[(Int, Token.Value, String)] = {
    // 如果剩余源码为空，EOF
    if (remainSourceCode.isEmpty) {
      Some(lineNum, Token.EOF, "EOF")
    } else {
      // 取 1 个字符
      remainSourceCode(0) match {
        case '$' =>
          skipSourceCodeChars(1)
          Some(lineNum, Token.VAR_PREFIX, "$")
        case '(' =>
          skipSourceCodeChars(1)
          Some(lineNum, Token.LEFT_PAREN, "(")
        case ')' =>
          skipSourceCodeChars(1)
          Some(lineNum, Token.RIGHT_PAREN, ")")
        case '=' =>
          skipSourceCodeChars(1)
          Some(lineNum, Token.EQUAL, "=")
        case '"' => // 如果是字符串，优先判断是否为空字符串
          if (nextSourceCodeIs("\"\"")) {
            // 如果是空字符串，直接跳过两个字符
            skipSourceCodeChars(2)
            Some(lineNum, Token.DUOQUOTE, "\"\"")
          } else {
            // 如果是一个引号，正常处理
            skipSourceCodeChars(1)
            Some(lineNum, Token.QUOTE, "\"")
          }
        case s@_ => if (s == '_' || isLetter(s)) {
          // 扫描名称
          scanName() match {
            case (token, Some(tokenType)) => Some(lineNum, tokenType, token)
            case (token, None) => Some(lineNum, Token.NAME, token)
          }
        } else if (nextIsNotSource) {
          // 如果没有源码，标记为 Ignored
          Some(lineNum, Token.IGNORED, s.toString)
        } else {
          printf("MatchToken(): unexpected symbol near '%c' at line %n.", s, lineNum)
          None
        }
      }
    }
  }

  /**
   * 扫描，直到 Token
   *
   * @param target 目标
   * @return 扫描结果
   */
  def scanUntilToken(target: String): Option[String] = {

    /**
     * 内部扫描
     *
     * @param length 扫描到的长度
     * @return 扫描结果
     */
    @tailrec def innerScan(length: Int): Option[String] = {
      if (length >= remainSourceCode.length) {
        // 如果已经到代码的最后还没找到目标 Token
        printf("scanBeforeToken(): expect a(n) %s.", target)
        None
      } else if (remainSourceCode.startsWith(target, length)) {
        // 如果已经找到了目标 Token
        val str = remainSourceCode.substring(0, length)
        skipSourceCodeChars(length)
        Some(str)
      } else {
        // 没找到时持续 + 1
        innerScan(length + 1)
      }
    }

    innerScan(0)
  }


  /**
   * 获取下一个 Token
   *
   * @return 下一个 Token
   */
  def GetNextToken(): Option[(Int, Token, String)] = {
    if (nextTokenLineNum > 0) {
      lineNum = nextTokenLineNum
      nextTokenLineNum = 0
      Some(lineNum, nextTokenType, nextToken)
    } else {
      MatchToken()
    }
  }

  /**
   * 寻找下一个指定的 Token
   *
   * @param token 目标 Token
   * @return 如果找到了，返回结果
   */
  def FindAndMoveToNextTokenWithType(token: Token): Option[(Int, String)] = {
    GetNextToken() match {
      case Some((nowLineNum, nowTokenType, nowToken)) if nowTokenType == token => Some(nowLineNum, nowToken)
      case Some((_, nowTokenType, nowToken)) =>
        printf("findNextTokenWithType(): syntax error near '%s', expected token: {%s} but got {%s}.", nowToken, token, nowTokenType)
        None
      case None => None
    }
  }

  /**
   * 寻找下一个任意 Token
   *
   * @return 任意 Token
   */
  def LookAhead(): Option[Token] = {
    if (nextTokenLineNum > 0) {
      // 如果已经寻找过下一个 Token，直接返回缓存
      Some(nextTokenType)
    } else {
      val nowLineNum = lineNum
      GetNextToken() match {
        case Some((lineNum, tokenType, token)) =>
          this.lineNum = nowLineNum
          this.nextTokenType = tokenType
          this.nextToken = token
          this.nextTokenLineNum = lineNum
          Some(tokenType)
        case None => None
      }
    }
  }

  /**
   * 仅寻找下一个目标 Token，并忽略之前的全部内容
   *
   * @param expectedTokenType 目标 Token 类型
   */
  def LookAheadAndSkip(expectedTokenType: Token): Unit = {
    val nowLineNum = lineNum
    GetNextToken() match {
      case Some((lineNum, tokenType, token)) if tokenType != expectedTokenType =>
        this.lineNum = nowLineNum
        this.nextTokenType = tokenType
        this.nextToken = token
        this.nextTokenLineNum = lineNum
      case _ => ()
    }
  }

}
