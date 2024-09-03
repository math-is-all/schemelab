;; 自定义 string-contains? 函数
(define (string-contains? str substr)
  (let loop ((i 0))
    (cond
      ((> i (- (string-length str) (string-length substr))) #f)
      ((string-prefix? substr (substring str i)) #t)
      (else (loop (+ i 1))))))

;; 检查是否保留该行的函数
(define (filter-line line)
  (not (string-contains? line "[")))

;; 读取并过滤文件内容
(define (remove-language-specific-lines input-port)
  (let loop ((line (read-line input-port)) (filtered-lines '()))
    (if (eof-object? line)
        (reverse filtered-lines)
        (let ((next-lines (if (filter-line line)
                              (cons line filtered-lines)
                              filtered-lines)))
          (loop (read-line input-port) next-lines)))))

;; 将过滤后的内容写入输出文件
(define (write-filtered-lines output-port lines)
  (for-each (lambda (line)
              (display line output-port)
              (newline output-port))
            lines))

;; 主处理函数
(define (process-files input-file output-file)
  (let ((input-port (open-input-file input-file))
        (output-port (open-output-file output-file)))
    (let ((filtered-lines (remove-language-specific-lines input-port)))
      (write-filtered-lines output-port filtered-lines))
    (close-input-port input-port)
    (close-output-port output-port)))

;; 调用主函数，处理指定的文件
(process-files "inputfile.txt" "outputfile.txt")

