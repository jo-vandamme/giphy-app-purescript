module Giphy.Data.Gifs (GifImage, Gif, Pagination, giphyResponseCodec) where

import Prelude
import Data.Codec (mapCodec, (>~>))
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Migration as CAM
import Data.Codec.Argonaut.Record as CAR
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Number (fromString)
import Giphy.Data.PaginatedArray (PaginatedArray)

type GifImage
  = { height :: Number
    , width :: Number
    , url :: String
    }

type Gif
  = { id :: String
    , title :: String
    , fixedWidth :: GifImage
    }

type Pagination
  = { totalCount :: Int
    , count :: Int
    , offset :: Int
    }

gifCodec :: JsonCodec Gif
gifCodec = mapCodec to from codec
  where
  codec =
    CAR.object "Gif"
      { id: CA.string
      , title: CA.string
      , images:
          CAR.object "Images"
            { fixed_width:
                CAR.object "GifImage"
                  { height: CA.prismaticCodec fromString show CA.string
                  , width: CA.prismaticCodec fromString show CA.string
                  , url: CA.string
                  }
            }
      }

  to gifJson =
    Right
      { id: gifJson.id
      , title: gifJson.title
      , fixedWidth: gifJson.images.fixed_width
      }

  from gif =
    { id: gif.id
    , title: gif.title
    , images:
        { fixed_width: gif.fixedWidth
        }
    }

paginationCodec :: JsonCodec Pagination
paginationCodec = CAM.renameField "total_count" "totalCount" >~> codec
  where
  codec =
    CAR.object "Pagination"
      { totalCount: CA.int
      , count: CA.int
      , offset: CA.int
      }

giphyResponseCodec :: JsonCodec (PaginatedArray Gif)
giphyResponseCodec =
  CAM.renameField "data" "body"
    >~> CAM.renameField "pagination" "total"
    >~> CAR.object "GiphyResponse"
        { body: CA.array gifCodec
        , total:
            CA.prismaticCodec
              (\x -> Just x.totalCount)
              (\t -> { totalCount: t, count: 0, offset: 0 })
              paginationCodec
        }
