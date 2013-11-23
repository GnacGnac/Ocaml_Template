
let rec concats = function
  | [] -> ""
  | [filename] -> filename
  | filename :: filenames -> Filename.concat filename (concats filenames)
