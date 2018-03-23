000100960221     H DECEDIT('0,') DATEDIT(*DMY.)
000200960221?     *--------------------------------------------------------------*
000300021010      * BAK850U1                                                     *
000400960221      *  *---------------------------------------------------------* *
000500021010      *    controlla quali records di KPLOG00F                       *
000600021010      *    l'utente può leggere in base alla sua abilitazione        *
000700021010      *    può leggere SE:                                           *
000800021010      *    - il profilo è uguale al suo                              *
000900021010      *                 è della sua area e lui è un POC            * *
001000021010      *                 lui è un EDP                               * *
001100021010     Fazute01l  IF   E           K DISK
001200021010     f                                             prefix(AZU:3)
001300021010      * P.O. gestibili dall'utente
001400021010     D $POG            s              3    dim(250) inz(*zeros)
001500021010av1  D DSLOG         E DS                  extname(kplog00f)
001600021010     D tibs34ds      E DS                  inz
001700021010     D ddatiute      e ds
001800021010     D azuteds       e ds                  extname(AZUTE00F)
001900021010     D DUTE01        E DS
002000021010     D TRUL31ds      E DS                  inz
002100021010     D KPJBA         E DS
002200960221?     *--------------------------------------------------------------*
002300960221?     *  CICLO PRINCIPALE                                            *
002400960221?     *--------------------------------------------------------------*
002500021011     C                   MOVEL     LOGKPJ        kpjba
002600021010     c                   reset                   $POG
002700021010     C                   SETOFF                                       58
002800021011     c* POC
002900021010     C     UTEFUN        IFEQ      'POC'
003000021010     c                   clear                   TRUL31DS
003100021010     c                   movel     'AR'          I31ABI
003200021010     c                   movel     DUTPOU        I31CPO
003300021010     c                   call      'TRUL31R'
003400021010     c                   parm                    KPJBA
003500021010     c                   parm                    TRUL31ds
003600021011if  3c                   If        O31pog > *zeros
003700021010     c                   movea     O31pog        $POG
003800021011e   3c                   Endif
003900021010     c     KNMUS         chain     azute01l                           54
004000021011     C  n54              MOVEL     AZUfil        WUTfil            3
004100021011     C   54              MOVEL     '999'         WUTfil
004200021011     c     WUTFIL        LOoKUP    $pog                                   58
004300021011e   3c                   Endif
004400021010     C*
004500021010     C                   MOVEL     utefaf        dute01
004600021010     c     utefun        ifeq      'EDPSE'
004700021010     c     KNMUS         OReq      DUTUTE
004800021010     c     *in58         oreq      '1'
004900021010     c                   movel     *blank        OKKO
005000021010     c                   endif
005100960221      *
005200960221     C                   SETON                                        LR
005300960221?     *--------------------------------------------------------------*
005400960221?     *  *INZSR: Operazioni iniziali                                 *
005500960221?     *--------------------------------------------------------------*
005600960221     C     *INZSR        BEGSR
005700021011      *
005800960221     C     *ENTRY        PLIST
005900021010     C                   PARM                    dslog
006000021010     C                   PARM                    OKKO                           KO = no
006100021011      *
006200021010     c                   movel     'KO'          OKKO              2
006300021011      *
006400021010     c     *dtaara       define    §azute        azuteds
006500021010     c     *dtaara       define    §datiute      ddatiute
006600021010      *
006700021010     c                   IN(E)     *DTAARA
006800021010if  1c                   IF        %Error
006900021010     c                   EVAL      I34Tla = 'L'
007000021010     c                   CALL      'TIBS34R'
007100021010     c                   PARM                    Tibs34Ds
007200021010     c                   IN        *DTAARA
007300021010     c                   ENDIF
007400021011      *
007500960221     C                   ENDSR
