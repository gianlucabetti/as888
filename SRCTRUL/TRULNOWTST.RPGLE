000100060908     H DFTACTGRP(*NO) BNDDIR('TRUL')
000200060908
000300060908     D*--------------------------------------------------
000400060908     D* Procedure name: now
000500060908     D* Purpose:        Restituisce un timestamp completo.
000600060908     D* Returns:
000700060908     D*--------------------------------------------------
000800060908     D now             PR              Z
000900060908
001000060908     D z               S               Z
001100060908
001200060908     C                   EVAL      z = now()
001300060908     C     z             DSPLY
001400060908     C                   EVAL      *INLR = *ON
