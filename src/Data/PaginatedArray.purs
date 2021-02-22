module Giphy.Data.PaginatedArray where

type PaginatedArray a
  = { total :: Int
    , body :: Array a
    }
