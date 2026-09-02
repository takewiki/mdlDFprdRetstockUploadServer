#' 处理逻辑
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param dms_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' prdRetstockUploadServer()
prdRetstockUploadServer <- function(input,output,session,dms_token) {

  options(shiny.maxRequestSize = 30 * 1024^2)
  #获取参数
  text_prdRetstock_upload = tsui::var_file('text_prdRetstock_upload')

  shiny::observeEvent(input$btn_prdRetstock_upload,{

    filename=text_prdRetstock_upload()

    if(filename==''  || is.null(filename)){

      tsui::pop_notice("请先上传文件")

    }else{

      # 清空临时表
      mdlDFprdRetstockUploadPkg::prdRetstock_delete(dms_token = dms_token)

      # 读取数据
      data <- readxl::read_excel(filename,col_types = c("text", "text", "text", "text",
                                                        "text", "text", "text", "text", "text",
                                                        "text", "text", "text", "text", "text",
                                                        "text", "text", "numeric", "text",
                                                        "text", "text", "numeric"))
      data <- as.data.frame(data)
      data <- tsdo::na_standard(data)

      batch_size <- 500
      total_rows <- nrow(data)
      total_batches <- ceiling(total_rows / batch_size)

      # 创建进度对象
      progress <- shiny::Progress$new()
      progress$set(message = "数据上传中", value = 0)
      on.exit(progress$close())

      # 初始化日志
      log_text <- reactiveVal("")

      # 记录总开始时间
      total_start_time <- Sys.time()

      # 初始化累计时间（分钟）
      cumulative_time <- 0

      # 更新进度回调
      update_progress <- function(value, detail = NULL) {
        progress$set(value = value, detail = detail)
      }

      for (i in 1:total_batches) {
        start_row <- (i - 1) * batch_size + 1
        end_row <- min(i * batch_size, total_rows)
        batch_data <- data[start_row:end_row, ]

        # 记录当前批次开始时间
        batch_start_time <- Sys.time()

        # 更新进度 - 显示详细信息（时间用分钟）
        if (i == 1) {
          detail_msg <- sprintf("正在上传第 %d/%d 批 (已用: 0.0分钟)", i, total_batches)
        } else {
          # 计算已用时间（分钟）
          elapsed_time <- as.numeric(difftime(Sys.time(), total_start_time, units = "mins"))
          # 计算预计剩余时间（分钟）
          if (i > 1) {
            avg_time_per_batch <- cumulative_time / (i - 1)
            remaining_batches <- total_batches - i + 1
            estimated_remaining <- avg_time_per_batch * remaining_batches
            detail_msg <- sprintf("正在上传第 %d/%d 批 | 已用: %.1f分钟 | 预计剩余: %.1f分钟",
                                  i, total_batches, elapsed_time, estimated_remaining)
          } else {
            detail_msg <- sprintf("正在上传第 %d/%d 批 (已用: %.1f分钟)", i, total_batches, elapsed_time)
          }
        }
        update_progress(i/total_batches, detail_msg)

        # 执行上传
        tsda::mysql_writeTable2(token = dms_token,table_name = 'rds_erp_byd_src_t_prd_retstock_list_input',r_object = batch_data,append = TRUE)


        # 计算当前批次耗时（分钟）
        batch_duration <- difftime(Sys.time(), batch_start_time, units = "mins")
        cumulative_time <- cumulative_time + as.numeric(batch_duration)

        # 计算总已用时间（分钟）
        total_elapsed <- as.numeric(difftime(Sys.time(), total_start_time, units = "mins"))

        # 追加日志（时间用分钟显示）
        log_text(paste0(log_text(),
                        sprintf("批次 %2d/%d: %d 条，耗时 %.2f 分钟 | 累计: %.1f分钟\n",
                                i, total_batches, nrow(batch_data),
                                as.numeric(batch_duration), total_elapsed)))

        # 更新日志显示
        output$upload_log <- renderPrint({
          cat(log_text())
        })
      }

      # 计算总耗时（分钟）
      total_duration <- as.numeric(difftime(Sys.time(), total_start_time, units = "mins"))

      # 追加完成日志
      log_text(paste0(log_text(),
                      sprintf("\n✅ 全部上传完成！\n")))
      log_text(paste0(log_text(),
                      sprintf("总记录数: %d 条 | 总批次: %d 批 | 总耗时: %.2f分钟\n",
                              total_rows, total_batches, total_duration)))
      log_text(paste0(log_text(),
                      sprintf("平均每批耗时: %.4f分钟\n", total_duration/total_batches)))

      output$upload_log <- renderPrint({
        cat(log_text())
      })

      # 插入list表和表头表体
      mdlDFprdRetstockUploadPkg::prdRetstock_insert(dms_token = dms_token)

      tsui::pop_notice("上传成功")
    }
  })
}

#' 处理逻辑
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param dms_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' prdRetstockViewServer()
prdRetstockViewServer <- function(input,output,session,dms_token) {

  #获取参数
  text_prdRetstock_daterange = tsui::var_dateRange('text_prdRetstock_daterange')

  shiny::observeEvent(input$btn_prdRetstock_view,{

    FDate = text_prdRetstock_daterange()

    FStartDate = FDate[1]

    FEndDate = FDate[2]

    data = mdlDFprdRetstockUploadPkg::prdRetstock_select(dms_token = dms_token,FStartDate =FStartDate ,FEndDate = FEndDate)

    tsui::run_dataTable2(id = 'prdRetstock_resultView',data = data)

    tsui::run_download_xlsx(id = 'dl_prdRetstock',data = data,filename = 'BYD生产退库.xlsx')




  })



}


#' 处理逻辑
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param dms_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' prdRetstockServer()
prdRetstockServer <- function(input,output,session,dms_token) {

  prdRetstockUploadServer(input = input,output = output,session = session,dms_token = dms_token)



  prdRetstockViewServer(input = input,output = output,session = session,dms_token = dms_token)


}
