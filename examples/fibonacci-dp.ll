; ModuleID = 'fibonacci-dp'
source_filename = "<string>"

%List = type { i8, [16 x i8] }
%ConsPair = type { i64, %List* }

declare void @printNum(i64)

declare i8* @malloc(i64)

declare i32 @free(...)

define %List @cons(i64 %h_0, %List %r_0) {
entry_0:
  %0 = alloca i64, align 8
  store i64 %h_0, i64* %0, align 8
  %1 = alloca %List, align 8
  store %List %r_0, %List* %1, align 8
  %2 = alloca %List, align 8
  %3 = getelementptr %List, %List* %2, i32 0, i32 1
  %4 = bitcast [16 x i8]* %3 to %ConsPair*
  %5 = load i64, i64* %0, align 8
  %6 = insertvalue %ConsPair undef, i64 %5, 0
  %7 = call i8* @malloc(i64 17)
  %8 = bitcast i8* %7 to %List*
  %9 = load %List, %List* %1, align 8
  store %List %9, %List* %8, align 8
  %10 = insertvalue %ConsPair %6, %List* %8, 1
  store %ConsPair %10, %ConsPair* %4, align 8
  %11 = getelementptr %List, %List* %2, i32 0, i32 0
  store i8 0, i8* %11, align 8
  %12 = load %List, %List* %2, align 8
  ret %List %12
}

define %List @empty() {
entry_0:
  %0 = alloca %List, align 8
  %1 = getelementptr %List, %List* %0, i32 0, i32 1
  %2 = bitcast [16 x i8]* %1 to i1*
  store i1 false, i1* %2, align 8
  %3 = getelementptr %List, %List* %0, i32 0, i32 0
  store i8 1, i8* %3, align 8
  %4 = load %List, %List* %0, align 8
  ret %List %4
}

define %List @make-fibs(i64 %n_0) {
entry_0:
  %0 = alloca i64, align 8
  store i64 %n_0, i64* %0, align 8
  %1 = load i64, i64* %0, align 8
  %2 = icmp sgt i64 %1, 2
  br i1 %2, label %if.then_0, label %if.else_0

if.then_0:                                        ; preds = %entry_0
  %3 = load i64, i64* %0, align 8
  %4 = sub i64 %3, 1
  %5 = call %List @make-fibs(i64 %4)
  %6 = alloca %List, align 8
  store %List %5, %List* %6, align 8
  %7 = load %List, %List* %6, align 8
  %8 = call i64 @nth(%List %7, i64 0)
  %9 = load %List, %List* %6, align 8
  %10 = call i64 @nth(%List %9, i64 1)
  %11 = add i64 %8, %10
  %12 = alloca i64, align 8
  store i64 %11, i64* %12, align 8
  %13 = load i64, i64* %12, align 8
  %14 = load %List, %List* %6, align 8
  %15 = call %List @cons(i64 %13, %List %14)
  br label %if.then.ret_0

if.then.ret_0:                                    ; preds = %if.then_0
  br label %if.exit_2

if.else_0:                                        ; preds = %entry_0
  %16 = load i64, i64* %0, align 8
  %17 = icmp eq i64 %16, 0
  br i1 %17, label %if.then_1, label %if.else_1

if.then_1:                                        ; preds = %if.else_0
  %18 = alloca %List, align 8
  %19 = getelementptr %List, %List* %18, i32 0, i32 1
  %20 = bitcast [16 x i8]* %19 to i1*
  store i1 false, i1* %20, align 8
  %21 = getelementptr %List, %List* %18, i32 0, i32 0
  store i8 1, i8* %21, align 8
  %22 = load %List, %List* %18, align 8
  br label %if.then.ret_1

if.then.ret_1:                                    ; preds = %if.then_1
  br label %if.exit_1

if.else_1:                                        ; preds = %if.else_0
  %23 = load i64, i64* %0, align 8
  %24 = icmp eq i64 %23, 1
  br i1 %24, label %if.then_2, label %if.else_2

if.then_2:                                        ; preds = %if.else_1
  %25 = call %List @empty()
  %26 = call %List @cons(i64 1, %List %25)
  br label %if.then.ret_2

if.then.ret_2:                                    ; preds = %if.then_2
  br label %if.exit_0

if.else_2:                                        ; preds = %if.else_1
  %27 = call %List @empty()
  %28 = call %List @cons(i64 1, %List %27)
  %29 = call %List @cons(i64 1, %List %28)
  br label %if.else.ret_0

if.else.ret_0:                                    ; preds = %if.else_2
  br label %if.exit_0

if.exit_0:                                        ; preds = %if.else.ret_0, %if.then.ret_2
  %30 = phi %List [ %26, %if.then.ret_2 ], [ %29, %if.else.ret_0 ]
  br label %if.else.ret_1

if.else.ret_1:                                    ; preds = %if.exit_0
  br label %if.exit_1

if.exit_1:                                        ; preds = %if.else.ret_1, %if.then.ret_1
  %31 = phi %List [ %22, %if.then.ret_1 ], [ %30, %if.else.ret_1 ]
  br label %if.else.ret_2

if.else.ret_2:                                    ; preds = %if.exit_1
  br label %if.exit_2

if.exit_2:                                        ; preds = %if.else.ret_2, %if.then.ret_0
  %32 = phi %List [ %15, %if.then.ret_0 ], [ %31, %if.else.ret_2 ]
  ret %List %32
}

define i64 @nth(%List %l_0, i64 %n_0) {
entry_0:
  %0 = alloca %List, align 8
  store %List %l_0, %List* %0, align 8
  %1 = alloca i64, align 8
  store i64 %n_0, i64* %1, align 8
  %2 = load %List, %List* %0, align 8
  %3 = extractvalue %List %2, 0
  %4 = icmp eq i8 0, %3
  br i1 %4, label %match.matched_0, label %match.not-matched_0

match.matched_0:                                  ; preds = %entry_0
  %5 = alloca [16 x i8], align 8
  %6 = extractvalue %List %2, 1
  store [16 x i8] %6, [16 x i8]* %5, align 8
  %7 = bitcast [16 x i8]* %5 to %ConsPair*
  %8 = load i64, i64* %1, align 8
  %9 = icmp eq i64 %8, 0
  br i1 %9, label %if.then_0, label %if.else_0

if.then_0:                                        ; preds = %match.matched_0
  %10 = load %ConsPair, %ConsPair* %7, align 8
  %11 = alloca %ConsPair, align 8
  store %ConsPair %10, %ConsPair* %11, align 8
  %12 = getelementptr %ConsPair, %ConsPair* %11, i32 0, i32 0
  %13 = load i64, i64* %12, align 8
  br label %if.then.ret_0

if.then.ret_0:                                    ; preds = %if.then_0
  br label %if.exit_0

if.else_0:                                        ; preds = %match.matched_0
  %14 = load %ConsPair, %ConsPair* %7, align 8
  %15 = alloca %ConsPair, align 8
  store %ConsPair %14, %ConsPair* %15, align 8
  %16 = getelementptr %ConsPair, %ConsPair* %15, i32 0, i32 1
  %17 = load %List*, %List** %16, align 8
  %18 = load %List, %List* %17, align 8
  %19 = load i64, i64* %1, align 8
  %20 = sub i64 %19, 1
  %21 = call i64 @nth(%List %18, i64 %20)
  br label %if.else.ret_0

if.else.ret_0:                                    ; preds = %if.else_0
  br label %if.exit_0

if.exit_0:                                        ; preds = %if.else.ret_0, %if.then.ret_0
  %22 = phi i64 [ %13, %if.then.ret_0 ], [ %21, %if.else.ret_0 ]
  br label %match.ret_0

match.ret_0:                                      ; preds = %if.exit_0
  br label %match.exit_0

match.not-matched_0:                              ; preds = %entry_0
  br label %match.matched_1

match.matched_1:                                  ; preds = %match.not-matched_0
  %23 = alloca [16 x i8], align 8
  %24 = extractvalue %List %2, 1
  store [16 x i8] %24, [16 x i8]* %23, align 8
  %25 = bitcast [16 x i8]* %23 to i1*
  br label %match.ret_1

match.ret_1:                                      ; preds = %match.matched_1
  br label %match.exit_0

match.exit_0:                                     ; preds = %match.ret_1, %match.ret_0
  %26 = phi i64 [ %22, %match.ret_0 ], [ -1, %match.ret_1 ]
  ret i64 %26
}

define i32 @main() {
entry_0:
  %0 = call %List @make-fibs(i64 7)
  %1 = call i64 @nth(%List %0, i64 0)
  call void @printNum(i64 %1)
  ret i32 0
}
