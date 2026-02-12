;;; Increase the recursion limit to 10,000
(let ( (newRecursLimit 10000) )
   (if (< (recursLimit) newRecursLimit)
      (if (recursLimit newRecursLimit)
         (writef "- Recursion limit increased to {0:,d}\n" (list newRecursLimit))
         (write "- Failed to increase recursion limit."))
      (uwriteln! "- No need to adjust recursion limit.")))


