package com.firmiana.util

import java.sql.{ResultSet, ResultSetMetaData}
import java.util

/**
 * @author jiangxintong@chinamobile.com 2024/10/22  
 */
class ResultSetPrinter {

  def printResultSet(rs: ResultSet): Unit = {
    val resultSetMetaData = rs.getMetaData
    // 获取列数
    val ColumnCount = resultSetMetaData.getColumnCount
    // 保存当前列最大长度的数组
    val columnMaxLengths = new Array[Int](ColumnCount)
    // 缓存结果集,结果集可能有序,所以用ArrayList保存变得打乱顺序.
    val results = new util.ArrayList[Array[String]]()
    // 按行遍历
    while (rs.next) {
      // 保存当前行所有列
      val columnStr = new Array[String](ColumnCount)
      // 获取属性值.
      for (i <- 0 until ColumnCount) {
        // 获取一列
        columnStr(i) = rs.getString(i + 1)
        // 计算当前列的最大长度
        columnMaxLengths(i) = Math.max(
          Math.max(columnMaxLengths(i), columnStr(i).length),
          resultSetMetaData.getColumnName(i+1).length
        )
      }
      // 缓存这一行.
      results.add(columnStr)
    }
    printSeparator(columnMaxLengths)
    try {
      printColumnName(resultSetMetaData, columnMaxLengths)
      printSeparator(columnMaxLengths)
    } catch {
      case _: java.util.MissingFormatWidthException =>
        println("Empty set")
    }
    // 遍历集合输出结果
    val iterator = results.iterator
    var columnStr: Array[String] = null
    while (iterator.hasNext) {
      columnStr = iterator.next
      for (i <- 0 until ColumnCount)
        printf("| %-" + columnMaxLengths(i) + "s ", columnStr(i))
      println("|")
    }
    printSeparator(columnMaxLengths)
  }

  private def printColumnName(resultSetMetaData: ResultSetMetaData, columnMaxLengths: Array[Int]): Unit = {
    val columnCount = resultSetMetaData.getColumnCount
    for (i <- 0 until columnCount)
      printf("| %-" + columnMaxLengths(i) + "s ", resultSetMetaData.getColumnName(i + 1))
    println("|")
  }

  /**
   * 输出分隔符.
   *
   * @param columnMaxLengths 保存结果集中每一列的最长的字符串的长度.
   */
  private def printSeparator(columnMaxLengths: Array[Int]): Unit = {
    for (i <- columnMaxLengths.indices) {
      print("+-")
      for (_ <- 0 to columnMaxLengths(i))
        print("-")
    }
    println("+")
  }
}
