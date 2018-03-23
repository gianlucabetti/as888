000100960221     H DECEDIT('0,') DATEDIT(*DMY.)
000101051123     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200960221?     *--------------------------------------------------------------*
000300021010      * BAK850U3                                                     *
000400960221      *  *---------------------------------------------------------* *
000500021010      *    profilo abilitato a tutti i records di KPLOG           *
000501021010      *    ** abilita le funzioni di modifica)                    *
000600021010      *    - l'abilitazione viene data a utenti del gruppo EDPSE     *
000601021010      *    - oppure se il flag §UTEMAZ di abilitazione su AZUTE00f   *
000700021010      *      è = a S                                               * *
000701060105     Fparut00f  IF   E             DISK
000702060105     F                                     USROPN
000900960221?     *  DS                                                          *
001000960221     D KPJBA         E DS
001002021010     D tibs34ds      E DS                  inz
001003021010     D ddatiute      e ds
001004021010     D azuteds       e ds                  extname(AZUTE00F)
001102021010     D DUTE01        E DS
001103060105     D rtnCde          S              1
002100960221?     *--------------------------------------------------------------*
002200960221?     *  CICLO PRINCIPALE                                            *
002300960221?     *--------------------------------------------------------------*
003602051123f    C                   MOVEL     utefaf        dute01
003603021010     c     utefun        ifeq      'EDPSE'
003604021010     c     §UTEMAZ       oreq      'S'
003605060105     c                   movel     *OFF          rtnCde
003606051123     c                   else
003607060105     c                   movel     *ON           rtnCde
003608021010     c                   endif
005300960221      *
005400960221     C                   SETON                                        LR
005500960221?     *--------------------------------------------------------------*
005600960221?     *  *INZSR: Operazioni iniziali                                 *
005700960221?     *--------------------------------------------------------------*
005800960221     C     *INZSR        BEGSR
005801060105     c
005900960221     C     *ENTRY        PLIST
006000960221     C                   PARM                    KPJBA
006001060105     C                   PARM                    rtnCde
006002060105     C
006003021010     c     *dtaara       define    §azute        azuteds
006004021010     c     *dtaara       define    §datiute      ddatiute
006005021010      *
006006060105     c                   EVAL      rtnCde = *OFF
006007021010     c                   IN(E)     *DTAARA
006008060105     c
006009060105     c                   IF        %Error
006010060105     c                   OPEN(E)   parut00f
006011060105     c                   IF        %ERROR
006012060105     c                   EVAL      *INLR = *ON
006013060105     c                   RETURN
006014060105     c                   ENDIF
006015060105     c                   CLOSE(E)  parut00f
006016021010     c                   EVAL      I34Tla = 'L'
006017060105     c                   CALL(E)   'TIBS34R'
006018021010     c                   PARM                    Tibs34Ds
006019060105     c                   IF        %ERROR
006020060105     c                   EVAL      *INLR = *ON
006021060105     c                   RETURN
006022060105     c                   ENDIF
006023021010     c                   IN        *DTAARA
006024021010     c                   ENDIF
006025060105     c
006100960221     C                   ENDSR
