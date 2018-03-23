000100100223     hDECEDIT('0,') DATEDIT(*DMY.)
000600130122     FAZORG01L  IF   E           K DISK
000700120402     Ftabel00f  IF   E           K DISK
000900160216     FfNLVP53D  CF   E             WORKSTN
001000030116      *****************************************************************
001100030116     d KPJBA         E DS
001200120330     D Tibs36ds      E DS
001300160216     D fNLVP53DS     e DS                  inz
001301160212     D WLBDAT          DS
001302160212     D  G02DAT                 1      8  0
001303160212     D  G02INV                 9     16  0
001304160212     D  G02ERR                17     17
001305160212     D  G02TGI                18     22  0
002600030205      *
002700120521     d azuteds       e ds                  extname(azute00f)
002800120521     d ddatiute      e ds
002900120521     d tibs34ds      e ds                  inz
003200100617     d dataiso         s               d   datfmt(*iso)
003300100617     d dataeur         s               d   datfmt(*eur)
003600000000      *--------------------------------------------------------------*
003700000000      *             M A I N      L I N E
003800000000      *--------------------------------------------------------------*
003900120329     c                   exsr      srinz
004000000000      * Emetto il formato video
004100120329     C                   do        *hival
004200020308     C                   SETOFF                                       90
004400030825     C                   EXFMT     CN23D01
004500120329     C   KC              leave
004600000000      * Eseguo i  controlli sui campi immessi a video
004700000000     C                   EXSR      CONTR
004800120329     C   90              iter
004900000000      * Per errore generico riemetto il formato video
005000160212      * Richiamo programma di estrazione
005100020308     c                   if        *inkf
005200160216     c                   clear                   fNLVP53DS
005300030116      * compone il codice numerico
005800160212     c                   z-add     DalAmg        f53dti
005900160212     c                   z-add     AlAmg         f53dtf
006000160212     c                   z-add     d1PO          f53PO
006300160216     c                   movel(p)  fNLVP53DS     kpjbu
006700160216     c                   call      'FNLVP52C'
006800030124     c                   parm                    kpjba
007300160216     c                   LEAVE
007400020308     c                   endif
007500000000      *
007600120329     C                   enddo
007700030116      *
007800120329     C                   SETON                                        LR
007900120329      *--------------------------------------------------------------*
008000120329      * Routine per inizializzare i campi
008100120329      *--------------------------------------------------------------*
008200120329     c     srinz         begsr
008300160212     C                   z-add     999           d1PO
008301160212     c                   eval      d1dpo = 'Tutte le filiali'
008302160212     c                   clear                   d1ddal
008303160212     c                   clear                   d1dal
011100120329     c                   endsr
011200000000      *--------------------------------------------------------------*
011300000000      * Routine per i controlli sui campi del formato video          *
011400000000      *--------------------------------------------------------------*
011500000000     C     CONTR         BEGSR
011600000000      *
011700030116     c                   movea     '0000000000'  *in(41)
011701160212     c* filiale
011702160212     c                   if        d1po <> 999
011703160212     C     d1PO          chain     AZORG01l
011704160212    1C                   IF        %found(azorg01l) and orgfva = ' '
011705160212     c                             and orgfag = 'F'
011706160212     c                   movel     orgdes        d1dpo
011707160212     c                   else
011708160212     c                   clear                   d1dpo
011709160212     c                   seton                                        5090
011710160212     c                   end
011711160212     c                   else
011712160212     c                   eval      d1dpo = 'Tutte le filiali'
011713160212     c                   end
018600020624      * controllo immissione delle date
018700030116     C     d1DDAL        ifeq      0
018800020308     C                   SETON                                        4390
018900120329     C                   leavesr
019000030116     C                   Else
019100030116     C                   Z-ADD     d1DDAL        G02DAT
019200000000     C                   MOVEL     *BLANK        G02ERR
019300940926     C                   CALL      'XSRDA8'
019400000000     C                   PARM                    WLBDAT
019500000000     C     G02ERR        IFEQ      '1'
019600020308     C                   SETON                                        44  90
019700120329     C                   leavesr
019800000000     C                   ENDIF
019900030116     C                   Z-ADD     G02INV        DalAmg            8 0
020000030116     C                   Z-ADD     G02dat        D1ddal
020100020307     c                   endif
020700030825      *
020800020624      * Controllo data finale
020900030116     C     d1Dal         ifeq      0
021000030127     C                   Z-ADD     d1DDal        d1Dal
021100030127     C                   End
021200030825      *
021300030116     C                   Z-ADD     d1Dal         G02DAT
021400020624     C                   MOVEL     *BLANK        G02ERR
021500020624     C                   CALL      'XSRDA8'
021600020624     C                   PARM                    WLBDAT
021700020624     C     G02ERR        IFEQ      '1'
021800020624     C                   SETON                                        42  90
021900120329     C                   leavesr
022000020624     C                   ENDIF
022100030825     c*
022200030116     C                   Z-ADD     G02INV        AlAmG             8 0
022300030116     C                   Z-ADD     G02dat        d1dal
022400030127     c*
022500030116     C     DalAmg        ifgt      AlAmg
022600020624     C                   SETON                                        4590
022700120329     C                   leavesr
022800020624     C                   END
023800030129      *
023900120329     C                   ENDSR
024000120329      *****************************************************************
024100120329     c     *inzsr        begsr
024200120329      *****************************************************************
024300120329     C     *ENTRY        PLIST
024400120329     C                   PARM                    KPJBA
025500120521     c     *dtaara       define    §azute        azuteds
025600120521     c     *dtaara       define    §datiute      ddatiute
025700120521     c                   in(e)     *dtaara
025800120521     c                   if        %error  or rsut = *blanks
025900120521     c                   clear                   tibs34ds
026000120521     c                   call      'TIBS34R'
026100120521     c                   parm                    tibs34ds
026200120521     c                   in        *dtaara
026300120521     c                   endif
026400160212     c                   endsr
