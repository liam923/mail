; ModuleID = 'simple'
source_filename = "<string>"

declare void @printNum(i64)

declare i8* @malloc(i64)

declare i32 @free(...)

define i32 @main() {
entry_0:
  call void @printNum(i64 10)
  ret i32 0
}
