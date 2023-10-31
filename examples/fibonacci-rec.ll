; ModuleID = 'fibonacci-rec'
source_filename = "<string>"

declare void @printNum(i64)

declare i8* @malloc(i64)

declare i32 @free(...)

define i64 @fib(i64 %n_0) {
entry_0:
  %0 = alloca i64, align 8
  store i64 %n_0, i64* %0, align 8
  %1 = load i64, i64* %0, align 8
  %2 = icmp eq i64 %1, 0
  %3 = load i64, i64* %0, align 8
  %4 = icmp eq i64 %3, 1
  %5 = or i1 %2, %4
  br i1 %5, label %if.then_0, label %if.else_0

if.then_0:                                        ; preds = %entry_0
  br label %if.then.ret_0

if.then.ret_0:                                    ; preds = %if.then_0
  br label %if.exit_0

if.else_0:                                        ; preds = %entry_0
  %6 = load i64, i64* %0, align 8
  %7 = sub i64 %6, 1
  %8 = call i64 @fib(i64 %7)
  %9 = load i64, i64* %0, align 8
  %10 = sub i64 %9, 2
  %11 = call i64 @fib(i64 %10)
  %12 = add i64 %8, %11
  br label %if.else.ret_0

if.else.ret_0:                                    ; preds = %if.else_0
  br label %if.exit_0

if.exit_0:                                        ; preds = %if.else.ret_0, %if.then.ret_0
  %13 = phi i64 [ 1, %if.then.ret_0 ], [ %12, %if.else.ret_0 ]
  ret i64 %13
}

define i32 @main() {
entry_0:
  %0 = call i64 @fib(i64 6)
  call void @printNum(i64 %0)
  ret i32 0
}
