#lang scribble/manual
@require[@for-label[rchess
                    racket/base]]

@title{rchess}
@author{Stephen R. Foster}

@defmodule[rchess]

This is a toolkit for working with chess stuff -- primarily for the purposes of chess education.
It can render chess diagrams, has some basic FEN parsing capabilities,
and can fetch chess puzzles from the API at chessblunders.com.

It builds on top of the racket @racket[chess] package (https://docs.racket-lang.org/chess).

The vision is to add more puzzles, and more modalities for training.  Currently, the supported modality is to render out puzzles as images and to study them as flashcards.
 
@(require rchess)

@defproc[(image-chess [pieces (listof piece-symbol?)]) image?]{
  
  @codeblock{
    (image-chess 
      '(R N B Q K B N R
        P P P P P P P P
        _ _ _ _ _ _ _ _
        _ _ _ _ _ _ _ _
        _ _ _ _ _ _ _ _ 
        _ _ _ _ _ _ _ _
        p p p p p p p p
        r n b q k b n r))
  }

Produces:

  @(image-chess start-position)


Note that the @racket[chess] package provides many features for rendering.  This function exists 1) to render out a @racket[image?] instead of a @racket[pict?] and 2) to provide a super-simple way to do so.
}


@section{Puzzles}

@defproc[(random-chessblunder) hash?]{
  Fetches a random blunder from chessblunders.com.

  @codeblock{
    (chessblunder->image (random-chessblunder))
  }

  Produces something like:

  @(chessblunder->image (random-chessblunder))
}

@defproc[(chessblunder 
           [#:answer? answer? boolean? #f] 
           [id string?]) hash?]{
  Fetches a random blunder from chessblunders.com.  The fetched data is cached locally, so subsequent calls do not trigger a request.

  @codeblock{
    (chessblunder->image (chessblunder "557884cee13823b824cbae19"))
  }

  Produces something like (with the blundering move displayed at the top):

  @(chessblunder->image 
     (chessblunder "557884cee13823b824cbae19"))

  Note that not all data from the puzzle is shown -- e.g. whose move it is, whether a side can castle, en passant opportunities, etc.  That data is returned from chessblunders.com but isn't currently rendered (a low-hanging fruit for anyone who wants to contribute to rchess.)

  You can show the correct answer with:

  @codeblock{
    (chessblunder->image 
      #:answer? #t
      (chessblunder "557884cee13823b824cbae19"))
  }

  This gives:

  @(chessblunder->image 
     #:answer? #t
     (chessblunder "557884cee13823b824cbae19"))
}



