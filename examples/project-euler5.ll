; ModuleID = 'project-euler5'
source_filename = "<string>"

declare void @printNum(i64)

declare i8* @malloc(i64)

declare i32 @free(...)

define i64 @gcd(i64 %a_0, i64 %b_0) {
entry_0:
  %0 = alloca i64, align 8
  store i64 %a_0, i64* %0, align 8
  %1 = alloca i64, align 8
  store i64 %b_0, i64* %1, align 8
  %2 = load i64, i64* %1, align 8
  %3 = load i64, i64* %0, align 8
  %4 = icmp sgt i64 %2, %3
  br i1 %4, label %if.then_0, label %if.else_0

if.then_0:                                        ; preds = %entry_0
  %5 = load i64, i64* %1, align 8
  %6 = load i64, i64* %0, align 8
  %7 = call i64 @gcd(i64 %5, i64 %6)
  br label %if.then.ret_0

if.then.ret_0:                                    ; preds = %if.then_0
  br label %if.exit_1

if.else_0:                                        ; preds = %entry_0
  %8 = load i64, i64* %1, align 8
  %9 = icmp eq i64 %8, 0
  br i1 %9, label %if.then_1, label %if.else_1

if.then_1:                                        ; preds = %if.else_0
  %10 = load i64, i64* %0, align 8
  br label %if.then.ret_1

if.then.ret_1:                                    ; preds = %if.then_1
  br label %if.exit_0

if.else_1:                                        ; preds = %if.else_0
  %11 = load i64, i64* %1, align 8
  %12 = load i64, i64* %0, align 8
  %13 = load i64, i64* %1, align 8
  %14 = srem i64 %12, %13
  %15 = call i64 @gcd(i64 %11, i64 %14)
  br label %if.else.ret_0

if.else.ret_0:                                    ; preds = %if.else_1
  br label %if.exit_0

if.exit_0:                                        ; preds = %if.else.ret_0, %if.then.ret_1
  %16 = phi i64 [ %10, %if.then.ret_1 ], [ %15, %if.else.ret_0 ]
  br label %if.else.ret_1

if.else.ret_1:                                    ; preds = %if.exit_0
  br label %if.exit_1

if.exit_1:                                        ; preds = %if.else.ret_1, %if.then.ret_0
  %17 = phi i64 [ %7, %if.then.ret_0 ], [ %16, %if.else.ret_1 ]
  ret i64 %17
}

define i64 @lcm(i64 %a_0, i64 %b_0) {
entry_0:
  %0 = alloca i64, align 8
  store i64 %a_0, i64* %0, align 8
  %1 = alloca i64, align 8
  store i64 %b_0, i64* %1, align 8
  %2 = load i64, i64* %0, align 8
  %3 = load i64, i64* %1, align 8
  %4 = mul i64 %2, %3
  %5 = load i64, i64* %0, align 8
  %6 = load i64, i64* %1, align 8
  %7 = call i64 @gcd(i64 %5, i64 %6)
  %8 = sdiv i64 %4, %7
  ret i64 %8
}

define i64 @max(i64 %a_0, i64 %b_0) {
entry_0:
  %0 = alloca i64, align 8
  store i64 %a_0, i64* %0, align 8
  %1 = alloca i64, align 8
  store i64 %b_0, i64* %1, align 8
  %2 = load i64, i64* %0, align 8
  %3 = load i64, i64* %1, align 8
  %4 = icmp sgt i64 %2, %3
  br i1 %4, label %if.then_0, label %if.else_0

if.then_0:                                        ; preds = %entry_0
  %5 = load i64, i64* %0, align 8
  br label %if.then.ret_0

if.then.ret_0:                                    ; preds = %if.then_0
  br label %if.exit_0

if.else_0:                                        ; preds = %entry_0
  %6 = load i64, i64* %1, align 8
  br label %if.else.ret_0

if.else.ret_0:                                    ; preds = %if.else_0
  br label %if.exit_0

if.exit_0:                                        ; preds = %if.else.ret_0, %if.then.ret_0
  %7 = phi i64 [ %5, %if.then.ret_0 ], [ %6, %if.else.ret_0 ]
  ret i64 %7
}

define i64 @min(i64 %a_0, i64 %b_0) {
entry_0:
  %0 = alloca i64, align 8
  store i64 %a_0, i64* %0, align 8
  %1 = alloca i64, align 8
  store i64 %b_0, i64* %1, align 8
  %2 = load i64, i64* %0, align 8
  %3 = load i64, i64* %1, align 8
  %4 = icmp sgt i64 %2, %3
  br i1 %4, label %if.then_0, label %if.else_0

if.then_0:                                        ; preds = %entry_0
  %5 = load i64, i64* %1, align 8
  br label %if.then.ret_0

if.then.ret_0:                                    ; preds = %if.then_0
  br label %if.exit_0

if.else_0:                                        ; preds = %entry_0
  %6 = load i64, i64* %0, align 8
  br label %if.else.ret_0

if.else.ret_0:                                    ; preds = %if.else_0
  br label %if.exit_0

if.exit_0:                                        ; preds = %if.else.ret_0, %if.then.ret_0
  %7 = phi i64 [ %5, %if.then.ret_0 ], [ %6, %if.else.ret_0 ]
  ret i64 %7
}

define i32 @main() {
entry_0:
  %0 = alloca i64, align 8
  store i64 1, i64* %0, align 8
  %1 = alloca i64, align 8
  store i64 1, i64* %1, align 8
  br label %while.cond_0

while.cond_0:                                     ; preds = %while.body_0, %entry_0
  %2 = load i64, i64* %1, align 8
  %3 = icmp sle i64 %2, 20
  br i1 %3, label %while.body_0, label %while.exit_0

while.body_0:                                     ; preds = %while.cond_0
  %4 = load i64, i64* %0, align 8
  %5 = load i64, i64* %1, align 8
  %6 = call i64 @lcm(i64 %4, i64 %5)
  store i64 %6, i64* %0, align 8
  %7 = load i64, i64* %1, align 8
  %8 = add i64 %7, 1
  store i64 %8, i64* %1, align 8
  br label %while.cond_0

while.exit_0:                                     ; preds = %while.cond_0
  %9 = load i64, i64* %0, align 8
  call void @printNum(i64 %9)
  ret i32 0
}
