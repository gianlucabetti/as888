000100960221     H DECEDIT('0,') DATEDIT(*DMY.)
000200960221?     *--------------------------------------------------------------*
000300021010      * BAK850U1                                                     *
000400960221      *  *---------------------------------------------------------* *
000500021010      *    controlla quali records di KPLOG00F                       *
000600021010      *    l'utente può leggere in base alla sua abilitazione        *
000700021010      *    può leggere SE:                                           *
000800021010      *    - il profilo è uguale al suo                              *
000900110216      *                 è della sua area                           * *
001000021010      *                 lui è un EDP                               * *
001100021010     Fazute01l  IF   E           K DISK
001200021010     f                                             prefix(AZU:3)
001300051007     F                                     USROPN
001400021010      * P.O. gestibili dall'utente
001500021010     D $POG            s              3    dim(250) inz(*zeros)
001600021010av1  D DSLOG         E DS                  extname(kplog00f)
001700021010     D tibs34ds      E DS                  inz
001800021010     D ddatiute      e ds
001900021010     D azuteds       e ds                  extname(AZUTE00F)
002000021010     D DUTE01        E DS
002100021010     D TRUL31ds      E DS                  inz
002200021010     D KPJBA         E DS
002300960221?     *--------------------------------------------------------------*
002400960221?     *  CICLO PRINCIPALE                                            *
002500960221?     *--------------------------------------------------------------*
002600021011     C                   MOVEL     LOGKPJ        kpjba
002700021010     c                   reset                   $POG
002800021010     C                   SETOFF                                       58
002900021011     c* POC
003000021010     C     UTEFUN        IFEQ      'POC'
003100021010     c                   clear                   TRUL31DS
003200110216     c                   movel     'RA'          I31ABI
003300021010     c                   movel     DUTPOU        I31CPO
003400021010     c                   call      'TRUL31R'
003500021010     c                   parm                    KPJBA
003600021010     c                   parm                    TRUL31ds
003700021011if  3c                   If        O31pog > *zeros
003800021010     c                   movea     O31pog        $POG
003900021011e   3c                   Endif
004000021010     c     KNMUS         chain     azute01l                           54
004100021011     C  n54              MOVEL     AZUfil        WUTfil            3
004200021011     C   54              MOVEL     '999'         WUTfil
004300021011     c     WUTFIL        LOoKUP    $pog                                   58
004400021011e   3c                   Endif
004500021010     C*
004600021010     C                   MOVEL     utefaf        dute01
004700021010     c     utefun        ifeq      'EDPSE'
004800021010     c     KNMUS         OReq      DUTUTE
004900021010     c     *in58         oreq      '1'
005000051006     c                   movel     '0'           OKKO
005100021010     c                   endif
005200960221      *
005300960221     C                   SETON                                        LR
005400960221?     *--------------------------------------------------------------*
005500960221?     *  *INZSR: Operazioni iniziali                                 *
005600960221?     *--------------------------------------------------------------*
005700960221     C     *INZSR        BEGSR
005800021011      *
005900960221     C     *ENTRY        PLIST
006000021010     C                   PARM                    dslog
006100021010     C                   PARM                    OKKO                           KO = no
006200051007
006300051007     ** Se non trovo il file restituisco OK.
006400051007     C                   OPEN(E)   azute01l
006500051007     C                   IF        %ERROR AND %STATUS = 1217
006600051007     C                   EVAL      okko = '0'
006700051007     C                   EVAL      *INLR = *ON
006800051007     C                   RETURN
006900051007     C                   ENDIF
007000021011      *
007100051006     c                   movel     '1'           OKKO              1
007200021011      *
007300021010     c     *dtaara       define    §azute        azuteds
007400021010     c     *dtaara       define    §datiute      ddatiute
007500021010      *
007600021010     c                   IN(E)     *DTAARA
007700021010if  1c                   IF        %Error
007800021010     c                   EVAL      I34Tla = 'L'
007900021010     c                   CALL      'TIBS34R'
008000021010     c                   PARM                    Tibs34Ds
008100021010     c                   IN        *DTAARA
008200021010     c                   ENDIF
008300021011      *
008400960221     C                   ENDSR
