000100100223     hDECEDIT('0,') DATEDIT(*DMY.)
000200000000      *****************************************************************
000300131203      *       STAMPA lo STORICIZZATO dall'AUT all'AUT nel periodo
000400030116      *****************************************************************
000500131203     Ffnlvp11p  O    e             PRINTER oflind(*in10)
000600030116     d KPJBA         E DS
000700131203     D fnlvp14ds     e DS                  inz
000800101018     D*-----------
000900000000     D WLBDAT          DS
001000940926     D  G02DAT                 1      8  0
001100940926     D  G02INV                 9     16  0
001200940926     D  G02ERR                17     17
001300940926     D  G02TGI                18     22  0
001400131203     D*-----------
001500131203     D  CAMPO6A        S              6
001600131203     D  CAMPO8A        S              8
001700131203     D  FINE_DOCumento...
001800131203     d                 S              1
001900131203     D*-----------
002000131203     D WrkSqlCmd       S           1024
002100131203     D Tfnlvp11s     e DS
002200131127      *
002300100617     d dataiso         s               d   datfmt(*iso)
002400100617     d dataeur         s               d   datfmt(*eur)
002500131127     d data_dal        s              8S 0
002600131127      *
002700000000      *--------------------------------------------------------------*
002800000000      *             M A I N      L I N E
002900000000      *--------------------------------------------------------------*
003000120329     C                   SETON                                        LR
003100131203     c*
003200131203     c* compongo istruzione sql
003300131203     c                   exsr      istruz
003400131203     C/EXEC SQL
003500131203     C+ PREPARE S1 FROM :WrkSqlCmd
003600131203     C/END-EXEC
003700131203
003800131203     C/EXEC SQL
003900131203     C+ DECLARE A1 CURSOR FOR S1
004000131203     C/END-EXEC
004100131203
004200131203     C/EXEC SQL
004300131203     C+ OPEN A1
004400131203     C/END-EXEC
004500131203     C                   DOU       SqlCod <> 0
004600131203     C/EXEC SQL
004700131203     C+ FETCH NEXT FROM A1 INTO :tfnlvp11S
004800131203     C/END-EXEC
004900131203     C                   SELECT
005000131203     **
005100131203     C                   WHEN      SqlCod < 0
005200131203     C                   seton                                        H1
005300131203     c                   goto      fine
005400131203     c*
005500131203     c* a fine file Totali x rotture
005600131203     C                   WHEN      SqlCod = 100
005700131203     c                   leave
005800131203     c*
005900131203     C                   OTHER
006000131203      * salto pagina
006100131203     c                   IF        P11STIPO <> 'CT' and
006200131203     c                             P11STIPO <> 'RT' and
006300131203     c                             Fine_Documento = 'S'
006400131203     c                   eval            Fine_Documento = 'N'
006500131203     c                   seton                                        10
006600131203     c                   end
006700131203     c* DETTAGLIO
006800131203     c                   if        *in10
006900131203      *                 * ------------------ *
007000131203     c                   exsr      testata
007100131203      *                 * ------------------ *
007200131203     c                   setoff                                       10
007300131203     c                   end
007400131203      *
007500131203     c                   exsr      campi
007600131203      *
007700131203     c                   if        P11STIPO ='C'   or
007800131203     c                             P11STIPO ='R'
007900131203     c                   write     det
008000131203      *
008100131203     c                   elseIF    P11STIPO ='CT'
008200131203     c                   seton                                        44
008300131203     c                   write     totcon
008400131203     c                   setoff                                       44
008500131203     c                   eval            Fine_Documento = 'S'
008600131203      *
008700131203     c                   elseIF    P11STIPO ='RT'
008800131203     c                   seton                                        44
008900131203     c                   write     totrit
009000131203     c                   setoff                                       44
009100131203     c                   eval            Fine_Documento = 'S'
009200131203     c*
009300131203     c                   end
009400131203     c*
009500131203     C                   ENDSL
009600131203
009700131203     C                   ENDDO
009800131203     C/EXEC SQL
009900131203     C+ CLOSE A1
010000131203     C/END-EXEC
010100131203      *
010200131203     c     fine          tag
010300120329      *****************************************************************
010400120329     c     *inzsr        begsr
010500120329      *****************************************************************
010600120329     C     *ENTRY        PLIST
010700120329     C                   PARM                    KPJBA
010800120330     c*
010900131203     c                   movel     kpjbu         fnlvp14ds
011000131127      ***
011100131127      * Imposta la UDATE  da TIME
011200120330     C                   TIME                    W0140            14 0
011300120330     C                   MOVE      W0140         UDATE8            8 0
011400120330     c                   move      udate8        dataeur
011500120330     c                   move      dataeur       dataiso
011600120330     c                   move      dataiso       udate8g           8 0
011700131127     C                   Z-ADD     udate8g       AlAmG             8 0
011800131203      *
011900131204     C                   MOVE      udate8        WOGGI
012000131203     c                   movel     *ALL'-'       TRA
012100131203     c                   movel     *ALL'-'       TRA1
012200131203     c                   seton                                        10
012300131127      *
012400120329     c                   endsr
012500131203     C**************************************************************************
012600131203     C* mi compongo l'istruzione sql
012700131203     C**************************************************************************
012800131203     C     istruz        BEGSR
012900131203      **
013000131203     c* MI COMPONGO L'ISTRUZIONE SQL che legge il file TFNLVP11S
013100131203     C                   EVAL      WrkSqlCmd
013200131203     C                             =
013300131203     C                             'SELECT * from TFNLVP11S where'
013400131203     c                              + ' P11WDDC  between '
013500131203     c                              + %trim(%char(f14dti)) +  ' and '
013600131203     c                              + %trim(%char(f14dtf)) +  ' and '
013700131203     c                              + ' P11CAUT  between '
013800131203     c                              + %trim(%char(f14cda)) +  ' and '
013900131203     c                              + %trim(%char(f14ca))
014000131203     c                              + ' ORDER BY RRN(tfnlvp11s)  '
014100131203      **
014200131203     C                   ENDSR
014300131203     C**************************************************************************
014400131203      *
014500131203     C**************************************************************************
014600131203     C     testata       begSR
014700131203      **
014800131203     c                   eval      wrkNDC = P11WRKNDC
014900131204     C     *iso          move      p11wddc       dataiso
015000131204     C                   move      dataiso       dataeur
015100131204     C                   move      dataeur       WDDC
015200131203     c                   eval      apdPDR =  P11CAUT
015300131203     c                   eval      apdRSC =  P11APDRSC
015400131203      **
015500131203      *                 * ------------------ *
015600131203     c                   write     testa
015700131203     c                   write     tesDIS
015800131203      *                 * ------------------ *
015900131203      *
016000131203     C                   ENDSR
016100131203     C**************************************************************************
016200131203      *
016300131203     C**************************************************************************
016400131203     C     campi         begSR
016500131203      **
016600131203      *      VALORIZZA I CAMPI DA STAMPARE
016700131203     c                   if        P11STIPO ='C'   or
016800131203     c                             P11STIPO ='R'
016900131203      *
017000131203     c                   eval      V1STIPO   =  P11STIPO
017100131203     c                   eval      V1SRAG    =  P11SRAG
017200131203     c                   eval      V1SIND    =  P11SIND
017300131203     c                   eval      V1SLOC    =  P11SLOC
017400131203     c                   eval      V1SPKG    =  P11SPKG
017500131203     c                   eval      V1SVLM    =  P11SVLM
017600131203     c                   eval      V1DCMC    =  P11DCMC
017700131203     c                   eval      V1SORA    =  P11SORA
017800131203     c                   eval      V1SAAS    =  P11SAAS
017900131203     c                   eval      V1SLNP    =  P11SLNP
018000131203     c                   eval      V1SNRS    =  P11SNRS
018100131203     c                   eval      V1SNSP    =  P11SNSP
018200131203     c                   eval      V1STSP    =  P11STSP
018300131203     c                   eval      V1SPOE    =  P11SPOE
018400131203     c                   eval      V1SNSR    =  P11SNSR
018500131203     c                   eval      V1SNOR    =  P11SNOR
018600131203     c                   eval      V1SNRV    =  P11SNRV
018700131203     c                   eval      V1ESITO   =  P11ESIORA
018800131203     c                   eval      V1ERRO    =  P11ERRORI
018900131203      *
019000131203      *
019100131203     c                   elseIF    P11STIPO ='CT'  or
019200131203     c                             P11STIPO ='RT'
019300131203      *
019400131203     C                   EVAL      CAMPO6A   =  %subst(P11SIND:1:2)
019500131203     c                                       +  %subst(P11SIND:4:2)
019600131203     c                                       +  %subst(P11SIND:7:2)
019700131203     c                   MOVEL(P)  CAMPO6A       V1SORA
019800131203      *
019900131203     C                   EVAL      CAMPO8A   =  %subst(P11SIND:11:2)
020000131203     c                                       +  %subst(P11SIND:14:2)
020100131203     c                                       +  %subst(P11SIND:17:4)
020200131203     c                   MOVEL(P)  CAMPO8A       V1SDTA
020300131203     C                   EVAL      V1STIPCHI = P11SLOC
020400131203      *
020500131203     c                   end
020600131203      *
020700131203     C                   ENDSR
020800131203     C**************************************************************************
