; ModuleID = 'project-euler1'
source_filename = "<string>"

declare void @printNum(i64)

declare i8* @malloc(i64)

declare i32 @free(...)

define i32 @main() {
entry_0:
  %0 = alloca i64, align 8
  store i64 0, i64* %0, align 8
  %1 = alloca i64, align 8
  store i64 1, i64* %1, align 8
  br label %while.cond_0

while.cond_0:                                     ; preds = %if.exit_0, %entry_0
  %2 = load i64, i64* %1, align 8
  %3 = icmp slt i64 %2, 1000
  br i1 %3, label %while.body_0, label %while.exit_0

while.body_0:                                     ; preds = %while.cond_0
  %4 = load i64, i64* %1, align 8
  %5 = srem i64 %4, 3
  %6 = icmp eq i64 %5, 0
  %7 = load i64, i64* %1, align 8
  %8 = srem i64 %7, 5
  %9 = icmp eq i64 %8, 0
  %10 = or i1 %6, %9
  br i1 %10, label %if.then_0, label %if.else_0

if.then_0:                                        ; preds = %while.body_0
  %11 = load i64, i64* %0, align 8
  %12 = load i64, i64* %1, align 8
  %13 = add i64 %11, %12
  store i64 %13, i64* %0, align 8
  br label %if.then.ret_0

if.then.ret_0:                                    ; preds = %if.then_0
  br label %if.exit_0

if.else_0:                                        ; preds = %while.body_0
  br label %if.else.ret_0

if.else.ret_0:                                    ; preds = %if.else_0
  br label %if.exit_0

if.exit_0:                                        ; preds = %if.else.ret_0, %if.then.ret_0
  %14 = phi i1 [ false, %if.then.ret_0 ], [ false, %if.else.ret_0 ]
  %15 = load i64, i64* %1, align 8
  %16 = add i64 %15, 1
  store i64 %16, i64* %1, align 8
  br label %while.cond_0

while.exit_0:                                     ; preds = %while.cond_0
  %17 = load i64, i64* %0, align 8
  call void @printNum(i64 %17)
  ret i32 0
}
