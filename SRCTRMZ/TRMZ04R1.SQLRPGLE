000100011213      *PARMS DFTACTGRP(*YES)
000200011213      *PARMS COMMIT(*NONE) DATFMT(*ISO)
000300011212      *PARMS CLOSQLCSR(*ENDMOD)
000400011212      *PARMS DYNUSRPRF(*OWNER)
000500000000     H DECEDIT('0,') DATEDIT(*DMY.)
000600170222     h dftactgrp(*yes)
000700170222     F*----------------------------------------------------*
000800000000     F*                   STAMPA AUTOMEZZI                 *
000900000000     F*----------------------------------------------------*
001000170222     F*//MZAME00F  IF   E           K DISK
001100910301     FTABEL00F  IF   E           K DISK
001200160517     FTntbe01l  IF   E           K DISK
001300160831     Fazorg01l  IF   E           K DISK
001400170227     fANENT01L  if   e           k disk
001500170227     fANSIF01L  if   e           k disk
001600170222     Fwmzama0f  o    E             DISK    usropn
001700170222     FQSYSPRT   O    F  132        PRINTER usropn
001800170222      *----------------------------------------------------*
001900011213     D S               S             66    DIM(8) CTDATA PERRCD(1)              COSTANTI STAMPA
002000160809     D SEL             S              1  0 DIM(11)                              ORDINAMENTO
002100160809     D CMP             S              7    DIM(11) CTDATA PERRCD(1)             NOMI CAMPI
002200160809     D KEY             S              7    DIM(5)                               CAMPI PER KEYFLD
002300170222      *
002400170222     D*// cmd1            S              1    DIM(99) CTDATA PERRCD(99)
002500160901     D cmd2            S              1    DIM(99) CTDATA PERRCD(99)
002600170222     D cmd3            S              1    DIM(99) CTDATA PERRCD(99)
002700160901     D cmd4            S              1    DIM(99) CTDATA PERRCD(99)
002800160901     D cmd5            S              1    DIM(99) CTDATA PERRCD(99)
002900170222     D*// cmd6            S              1    DIM(99) CTDATA PERRCD(99)
003000000000     D*----------------------------------------------------*
003100170222     d coman           s             99
003200170222     d luncmd          s             15  5
003300170222     d file            s             10
003400170222     D WrkSqlCmd       S           1024
003500170227     d wSaveCAR        s                   inz  like( AMEcar )
003600170227     d wSaveCAT        s                   inz  like( AMEcat )
003700170227     d wSaveUSI        s                   inz  like( AMEusi )
003800170227     d wSaveSTA        s                   inz  like( AMEsta )
003900170227     d wSaveCSP        s                   inz  like( AMEcsp )
004000170227     d wSaveCRC        s                   inz  like( AMEcrc )
004100170227     d wSaveCFI        s                   inz  like( AMEcfi )
004200170227     d wDesXR          s             25    inz
004300000000     D*
004400000000     D PARAM           DS
004500011212     D  SCAT                          1
004600011212     D  SDIT                          1
004700011212     D  SFIL                          1
004800160809     D  keyfld                       35
004900011213     D  ANNOV                         4  0
005000011213     D  MESEV                         2  0
005100011212     D  VTGA                          2
005200011212     D  VTGN                          9
005300011212     D  VIMM                          4  0
005400011212     D  VCAT                          2
005500160517     D  Vcaut                         2
005600160517     D  Vmau                          3
005700011212     D  VDIT                          3
005800011212     D  VFIL                          3  0
005900011212     D  VCDC                          8
006000160809     D  Vdadug                        8  0
006100160809     D  Valdug                        8  0
006200160809     D  Vusodg                        8  0
006300160809     D  Vusoag                        8  0
006400170222     D  VfileSt                       1
006500170227     D  ParamSOC                      3
006600170222      *
006700160517     D  dmau         E DS
006800160517     D  dcau         E DS
006900170227     d  dsXA         e ds                  inz   qualified
007000170227     d  dsXB         e ds                  inz   qualified
007100170227     d  dsXD         e ds                  inz   qualified
007200170227     d  dsXF         e ds                  inz   qualified
007300170227     d  dsXH         e ds                  inz   qualified
007400160831     D  dsxn         E DS
007500170227      *
007600000000     D KPJBA         E DS
007700000000     D  LIBSYS                92    100
007800000000     D  MSGERR               453    502
007900170227      *
008000910218     D                 DS
008100910218     D  DATO                   1      4  0
008200910218     D  MESE                   1      2  0
008300910218     D  ANNO                   3      4  0
008400160809     D*-------------
008500160809     D WLBDA8          DS
008600160809     D  G02DAT                 1      8  0
008700160809     D  G02INV                 9     16  0
008800160809     D  G02ERR                17     17
008900160809     D  G02TGI                18     22  0
009000170222      *
009100011213     D mzame         E DS                  extname(mzame00f)
009200170227      *
009300170227      * -?Parametri x Controllo profilo utenti?
009400170227     d TIBS34ds      e ds
009500170227      * -?Ds di riferimento al file esterno AZUTE00F?
009600170227     d AZUTEds       e ds                  extname(AZUTE00F)
009700170227      * -?Ds per dati organigramma?
009800170227     d dDatiUte      e ds
009900000000     I/SPACE 3
010000000000     C*---------------------------------------------------------------*
010100170222      *
010200170222     c/exec sql
010300170222     c+ Set Option  CloSqlCsr = *EndMod,
010400170222     c+             Commit    = *None,
010500170222     c+             DatFmt    = *Iso,
010600170222     c+             DynUsrPrf = *Owner
010700170222     c/end-exec
010800011212     c*
010900011212     c                   if        not *inlr
011000011212     c* Lettura WRKAME00F
011100011212     C/EXEC SQL
011200011212     C+ PREPARE S1 FROM :WrkSqlCmd
011300011212     C/END-EXEC
011400011212
011500011212     C/EXEC SQL
011600011212     C+ DECLARE A1 CURSOR FOR S1
011700011212     C/END-EXEC
011800011212
011900011212     C/EXEC SQL
012000011212     C+ OPEN A1
012100011212     C/END-EXEC
012200011212
012300011212     C                   DOU       SqlCod <> 0
012400011212     C/EXEC SQL
012500011212     C+ FETCH NEXT FROM A1 INTO :mzame
012600011212     C/END-EXEC
012700011212     c*
012800011212     C                   SELECT
012900011212     C                   WHEN      SqlCod = 0
013000170222if  3c                   If        VfileSt = 'S'
013100011212     c* parametri
013200170222     c                   if        not *in99
013300011212     C                   EXCEPT    TES1
013400011212     C                   SETON                                            99
013500011212     c                   end
013600011212      *  STAMPA
013700011212     C   99              EXSR      XSALTO
013800011212     C  N99              SETON                                            99
013900170222e   3c                   EndIf
014000011212     C                   EXSR      XSALVA
014100011212     C     AMEDTP        COMP      0                                      16
014200011212     C                   SETOFF                                       50
014300011212     C* DECODIFICA AUTISTA
014400170119     C                   clear                   DESAUT           24
014500170119     C                   clear                   funz              3
014600030715     C     AMECAT        IFEQ      'A1'
014700030715     C     AMECAT        orEQ      'A2'
014800030715     C     AMECAT        orEQ      'U1'
014900030715     C     AMECAT        orEQ      'U2'
015000011212     C                   MOVEL     'XN'          COD
015100011212     C                   MOVEL     *BLANKS       KOY
015200011212     C                   MOVEL     AMECAU        KOY
015300160831     C                   clear                   dsxn
015400160831     C     KTAB          CHAIN     TABEL00f                           22
015500160831     C                   if        %found(tabel00f)
015600160831     C                   MOVEL     TBLUNI        Dsxn
015700160831     c                   endif
015800160831     C                   MOVEL     §xndes        DESAUT           24
015900160831     C                   MOVEL     §xnfun        funz              3
016000011212     C                   SETON                                        50
016100011212     C                   END
016200011212     C*
016300170222if  3c                   If        VfileSt = 'S'
016400011212     C     CONRIG        IFGT      60
016500011212     C                   EXCEPT    TESTA
016600011212     C                   Z-ADD     9             CONRIG
016700011212     C                   END
016800170222e   3c                   EndIf
016900160517     c                   clear                   amemaud          30
017000160517     c                   movel     'MAU'         tbecod
017100160517     c                   movel(p)  amemau        tbeke1
017200160517     c     ktbe          chain     tntbe01l
017300160517     c                   if        %found(tntbe01l)
017400160517     c                   movel     tbeuni        dmau
017500160517     c                   movel     §maudes       amemaud
017600160517     c                   endif
017700160517     c                   clear                   amecautd         30
017800160517     c                   movel     'CAU'         tbecod
017900160809     c                   movel(p)  amecaut       tbeke1
018000160517     c     ktbe          chain     tntbe01l
018100160517     c                   if        %found(tntbe01l)
018200160517     c                   movel     tbeuni        dcau
018300160517     c                   movel     §caudes       amecautd
018400160517     c                   endif
018500160809     c                   clear                   amedurs           8 0
018600160809     c                   if        amedur > 0
018700160809     C                   MOVE      amedur        G02inv
018800160809     C                   MOVEL     '3'           G02ERR
018900160809     C                   CALL      'XSRDA8'
019000160809     C                   PARM                    WLBDA8
019100160809     C     G02ERR        IFne      '1'
019200160809     c                   move      g02dat        amedurs
019300160809     C                   END
019400160809     c                   endif
019500160809     c                   clear                   amedals           8 0
019600160809     c                   if        amedal > 0
019700160809     C                   MOVE      amedal        G02inv
019800160809     C                   MOVEL     '3'           G02ERR
019900160809     C                   CALL      'XSRDA8'
020000160809     C                   PARM                    WLBDA8
020100160809     C     G02ERR        IFne      '1'
020200160809     c                   move      g02dat        amedals
020300160809     C                   END
020400160809     c                   endif
020500170222if  3c                   If        VfileSt = 'F'
020600160831     c                   exsr      scriviWF
020700170222e   3c                   EndIf
020800160809      *
020900170222if  3c                   If        VfileSt = 'S'
021000011212     C                   EXCEPT    DETAIL
021100011212     C                   ADD       3             CONRIG
021200170222e   3c                   EndIf
021300011212     C                   WHEN      SqlCod = 100
021400011212     C                   WHEN      SqlCod <> 0
021500011212      * Forzo la stampa del JOBLOG.
021600011212     C                   CALL      'X66CHGJOB'
021700011212     C                   ENDSL
021800011212     C                   ENDDO
021900011212
022000011212     C/EXEC SQL
022100011212     C+ CLOSE A1
022200011212     C/END-EXEC
022300011212     c                   end
022400011212     c*
022500170222if  1c                   If        VfileSt = 'S'
022600170222     C                   close     QsysPrt
022700170222e   1c                   EndIf
022800170222      *
022900170222if  1c                   If        VfileSt = 'F'
023000170222     c*
023100160901      * operazioni di fine duplica da file in qtemp a libreria utente
023200160901      *verifica se esiste file in libreria di wrk per utente
023300160901     c                   close     wmzama0f
023400160901      *
023500160901     c                   movea     cmd4          coman
023600160901     c                   eval      %subst(coman:22:10) = file
023700160901     c                   exsr      comandi
023800170222      * se trova il file nella libr. WRKQUERY lo cancella
023900170222     c                   if        NOT *in34
024000170222     c                   movea     cmd3          coman
024100170222     c                   eval      %subst(coman:20:10) = file
024200170222     c                   exsr      comandi
024300170222     c                   endif
024400170222      *//*se trova il file esegue copy file altrimenti lo crea
024500170222     c*//                   if        *in34
024600170222      * ...per poi eseguire copy-file
024700160901     c                   movea     cmd5          coman
024800160901     c                   eval      %subst(coman:78:10) = file
024900160901     c                   exsr      comandi
025000170222     c*//                   else
025100170222     c*//                   movea     cmd6          coman
025200170222     c*//                   eval      %subst(coman:41:10) = file
025300170222     c*//                   exsr      comandi
025400170222     c*//                   endif
025500170222     c*
025600170222e   1c                   EndIf
025700170222      *
025800011212     C                   seton                                        lr
025900160901     c*-------------------------------------------------
026000160901     c     comandi       begsr
026100160901     c*-------------------------------------------------
026200160901      *
026300160901     C                   z-add     99            luncmd
026400160901     C                   CALL      'QCMDEXC'                            34
026500160901     C                   parm                    coman
026600160901     C                   parm                    luncmd
026700160901      *
026800160901     c                   endsr
026900000000     C***------------------------------------------------***
027000000000     CSR   XSALVA        BEGSR                                                  *
027100011212     C                   MOVE      AMECAT        SAVCAT            2
027200011212     C                   move      AMEDIT        SAVDIT            3
027300011212     C                   move      AMEFIL        SAVFIL            3 0
027400000000     C                   ENDSR
027500000000     C***------------------------------------------------***
027600000000     C***   GESTISCE SALTO PAGINA E RIGA A ROTTURA   ----***
027700000000     C***------------------------------------------------***
027800000000     CSR   XSALTO        BEGSR                                                  *
027900000000      *
028000011212     c                   do
028100000000     C     SAVCAT        IFNE      AMECAT
028200000000     C     SCAT          ANDEQ     'P'
028300000000     C                   Z-ADD     70            CONRIG
028400011212     C                   leave
028500000000     C                   END
028600000000     C*
028700000000     C     SAVDIT        IFNE      AMEDIT
028800000000     C     SDIT          ANDEQ     'P'
028900000000     C                   Z-ADD     70            CONRIG
029000011212     C                   leave
029100000000     C                   END
029200000000     C*
029300000000     C     SAVFIL        IFNE      AMEFIL
029400000000     C     SFIL          ANDEQ     'P'
029500000000     C                   Z-ADD     70            CONRIG
029600011212     C                   leave
029700000000     C                   END
029800000000     **
029900000000     C     SAVCAT        IFNE      AMECAT
030000000000     C     SCAT          ANDEQ     'R'
030100000000     C                   EXCEPT    XSAL
030200011212     C                   leave
030300000000     C                   END
030400000000     **
030500000000     C     SAVDIT        IFNE      AMEDIT
030600000000     C     SDIT          ANDEQ     'R'
030700000000     C                   EXCEPT    XSAL
030800011212     C                   leave
030900000000     C                   END
031000000000     **
031100000000     C     SAVFIL        IFNE      AMEFIL
031200000000     C     SFIL          ANDEQ     'R'
031300000000     C                   EXCEPT    XSAL
031400011212     C                   leave
031500000000     C                   END
031600011212     C                   ENDdo
031700011212     C                   ENDSR
031800011212     c**********************************************************************
031900011212     c     srsql         begsr
032000011212     c**********************************************************************
032100011212     C                   EVAL      WrkSqlCmd
032200011212     C                             =
032300011212     C                             'SELECT'
032400011212     C                             +
032500011212     C                             ' * FROM MZAME00F '
032600011212     C                             +
032700011212     C                             ' WHERE AMENRF <> 0'
032800011212     c*scelta filiale
032900011212     C                   IF        vfil <> 0
033000011212     C                   EVAL      WrkSqlCmd
033100011212     C                             =
033200011212     C                             %TRIMR(WrkSqlCmd)
033300011212     C                             +
033400011212     C                             ' AND AMEFIL = ' + %editc(vfil:'Z')
033500011212     C                   endif
033600011212     c*scelta ditta
033700011212     C                   IF        vdit <> *blanks
033800011212     C                   EVAL      WrkSqlCmd
033900011212     C                             =
034000011212     C                             %TRIMR(WrkSqlCmd)
034100011212     C                             +
034200011212     C                             ' AND AMEDIT = ' + '''' + VDIT + ''''
034300011212     C                   endif
034400011212     c*scelta cdc
034500011212     C                   IF        vcdc <> *blanks
034600011212     C                   EVAL      WrkSqlCmd
034700011212     C                             =
034800011212     C                             %TRIMR(WrkSqlCmd)
034900011212     C                             +
035000011212     C                             ' AND AMECDC = ' + '''' + Vcdc + ''''
035100011212     C                   endif
035200011212     c*scelta categoria
035300011212     C                   IF        vcat <> ' '
035400050526     c                   if        %subst(vcat: 2: 1) = '*'
035500050526     c                   movel     vcat          comcat            1
035600050526     C                   EVAL      WrkSqlCmd
035700050526     C                             =
035800050526     C                             %TRIMR(WrkSqlCmd)
035900050526     C                             +
036000050526     C                             ' AND substr(AMECAT, 1, 1) = '
036100050526     C                             + '''' + COMCAT + ''''
036200050526     c                   else
036300011212     C                   EVAL      WrkSqlCmd
036400011212     C                             =
036500011212     C                             %TRIMR(WrkSqlCmd)
036600011212     C                             +
036700011212     C                             ' AND AMECAT = ' + '''' + VCAT + ''''
036800011212     C                   endif
036900050526     C                   endif
037000011212     c*scelta immatricolazione
037100011212     C                   IF        vimm <> 0
037200011212     C                   EVAL      WrkSqlCmd
037300011212     C                             =
037400011212     C                             %TRIMR(WrkSqlCmd)
037500011212     C                             +
037600011212     C                             ' AND AMEIMM = ' + %editc(vimm:'Z')
037700011212     C                   endif
037800160809     c*scelta data ultima revisione
037900160809     C                   IF        vdadug <> 0
038000160809     C                   EVAL      WrkSqlCmd
038100160809     C                             =
038200160809     C                             %TRIMR(WrkSqlCmd)
038300160809     C                             +
038400160809     C                             ' AND AMEDUR between ' + %editc(vdadug:'Z')
038500160809     C                             + ' AND ' + %editc(valdug:'Z')
038600160809     C                   endif
038700160809     c*scelta data in uso dal ...
038800160809     C                   IF        vusodg <> 0
038900160809     C                   EVAL      WrkSqlCmd
039000160809     C                             =
039100160809     C                             %TRIMR(WrkSqlCmd)
039200160809     C                             +
039300160809     C                             ' AND AMEDAL between ' + %editc(vusodg:'Z')
039400160809     C                             + ' AND ' + %editc(vusoag:'Z')
039500160809     C                   endif
039600011212     c*scelta targa
039700011212     C                   IF        vtga <> ' '
039800011212     C                   EVAL      WrkSqlCmd
039900011212     C                             =
040000011212     C                             %TRIMR(WrkSqlCmd)
040100011212     C                             +
040200011212     C                             ' AND AMETGA = ' + '''' + VTGA + ''''
040300011212     C                             +
040400011212     C                             ' AND AMETGN = ' + '''' + VTGN + ''''
040500011212     C                   endif
040600020206     c*scelta marca
040700160517     C                   IF        vcaut <> ' '
040800020206     C                   EVAL      WrkSqlCmd
040900020206     C                             =
041000020206     C                             %TRIMR(WrkSqlCmd)
041100020206     C                             +
041200160809     C                             ' AND AMECAUT = ' + '''' + VCAUT + ''''
041300020206     C                   endif
041400011212     c*scelta marca e tipo
041500160517     C                   IF        vmau <> ' '
041600011212     C                   EVAL      WrkSqlCmd
041700011212     C                             =
041800011212     C                             %TRIMR(WrkSqlCmd)
041900011212     C                             +
042000160517     C                             ' AND AMEMAU = ' + '''' + VMAU + ''''
042100011212     C                   endif
042200020206     c*
042300011212     C                   EVAL      WrkSqlCmd
042400011212     C                             =
042500011212     C                             %TRIMR(WrkSqlCmd)
042600011212     C                             +
042700011212     C                             ' ORDER BY '
042800011212     c* ORDINAMENTO
042900011212     C                   EVAL      WrkSqlCmd
043000011212     C                             =
043100011212     C                             %TRIMR(WrkSqlCmd)
043200011212     C                             + ' ' + key(1)
043300011212     c     2             do        5             aa                2 0
043400011212     c                   if        key(AA) <> 'AMENRF'
043500011212     C                   EVAL      WrkSqlCmd
043600011212     C                             =
043700011212     C                             %TRIMR(WrkSqlCmd)
043800011212     c                             +
043900011212     C                             ',' + key(aa)
044000011212     c                   end
044100011212     c                   enddo
044200011212     c                   if        key(1) <> 'AMENRF'
044300011212     C                   EVAL      WrkSqlCmd
044400011212     C                             =
044500011212     C                             %TRIMR(WrkSqlCmd)
044600011212     C                             +
044700011212     C                             ', AMENRF'
044800011212     c                   end
044900011212     c*
045000011212     C                   ENDSR
045100160831     c*_____________________________________________________
045200160831     c     scriviWF      begsr
045300160831     c*_____________________________________________________
045400170227     c                   clear                   WMZAMA0
045500170227      *
045600160831     c                   eval      WAMaFIL  =  AMEFIL
045700160831     C     AMEFIL        CHAIN     AZORG                              10
045800160831     C                   MOVEL     *BLANKS       wamafild
045900160831     C  N10              MOVEL     ORGDES        wamafild
046000170227     c                   eval      WAMaNRF  =  AMENRF
046100170227     c                   movel     AMEcdc        WAMAcdc
046200170227if  1c                   If        ENTentita   <> WAMAcdc
046300170227     c                   eval      ENTentita   =  WAMAcdc
046400170227     c     keyANENT01    chain     ANENT000
046500170227     c                   if        Not  %found( ANENT01L )
046600170227     c                   clear                   ENTdesBrev
046700170227     c                   endif
046800170227e   1c                   EndIf
046900170227     c                   eval      WAMAcdcD  = ENTdesBrev
047000170227     c                   eval      WAMAsoc   = AMEsoc
047100170227if  1c                   If        SIFsocieta  <> AMEsoc
047200170227     c                   eval      SIFsocieta  =  AMEsoc
047300170227     c     SIFsocieta    chain     ANSIF000
047400170227     c                   if        Not  %found( ANSIF01L )
047500170227     c                   clear                   SIFdesBrev
047600170227     c                   endif
047700170227e   1c                   EndIf
047800170227     c                   eval      WAMAsocD  = SIFdesBrev
047900170227     c*//                eval      WAMaTGA   = AMETGA
048000170227     c*//                eval      WAMaTGN   = AMETGN
048100170227     c                   eval      wAMAtarga = AMEtga + ' ' + AMEtgn
048200170227     c                   eval      WAMaTLA   = AMETLA
048300170227     c                   eval      WAMAcar   = AMEcar
048400170227if  1c                   If        AMEcar   <> *blanks
048500170227     c                   if        AMEcar   <> wSaveCAR
048600170227     c                   clear                   dsXB
048700170227     c                   eval      wSaveCAR  = AMEcar
048800170227     c                   eval      Cod       = 'XB'
048900170227     c                   eval      Koy       = AMEcar
049000170227     c     kTAB          chain     TABEL
049100170227     c                   if        %found( TABEL00F )
049200170227     c                   eval      dsXB      = TBLuni
049300170227     c                   endif
049400170227     c                   endif
049500170227     c                   eval      WAMAcarD  = dsXB.§XBdes
049600170227e   1c                   EndIf
049700170227     c                   eval      WAMaCAT   = AMECAT
049800170227if  1c                   If        AMEcat   <> *blanks
049900170227     c                   if        AMEcat   <> wSaveCAT
050000170227     c                   clear                   dsXA
050100170227     c                   eval      wSaveCAT  = AMEcat
050200170227     c                   eval      Cod       = 'XA'
050300170227     c                   eval      Koy       = AMEcat
050400170227     c     kTAB          chain     TABEL
050500170227     c                   if        %found( TABEL00F )
050600170227     c                   eval      dsXA      = TBLuni
050700170227     c                   endif
050800170227     c                   endif
050900170227     c                   eval      WAMAcatD  = dsXA.§XAdes
051000170227e   1c                   EndIf
051100170227     c                   eval      WAMaCAUt  = AMECAUt
051200170227     c                   eval      WAMaCAUtd = AMECAUtd
051300160831     c                   eval      WAMamau  =  AMEmau
051400160831     c                   eval      WAMamaud =  AMEmaud
051500170227     c                   eval      WAMaCAU  =  AMECAU
051600160831     c                   eval      WAMaCAUD =  desaut
051700170227     c                   eval      WAMafun  =  funz
051800160831     c                   eval      WAMaDAL  =  AMEDAL
051900160831     c                   eval      WAMADIT  =  AMEDIT
052000170227if  1c                   If        SIFsocieta  <> AMEdit
052100170227     c                   eval      SIFsocieta  =  AMEdit
052200170227     c     SIFsocieta    chain     ANSIF000
052300170227     c                   if        Not  %found( ANSIF01L )
052400170227     c                   clear                   SIFdesBrev
052500170227     c                   endif
052600170227e   1c                   EndIf
052700170227     c                   eval      WAMAditD  = SIFdesBrev
052800170227     c                   eval      WAMAfor   = AMEfor
052900170227     c                   eval      WAMAimm   = AMEimm
053000170227     c                   eval      WAMAali   = AMEali
053100170227     c                   eval      WAMAcil   = AMEcil
053200170227     c                   eval      WAMAusi   = AMEusi
053300170227if  1c                   If        AMEusi   <> *blanks
053400170227     c                   if        AMEusi   <> wSaveUSI
053500170227     c                   clear                   dsXH
053600170227     c                   eval      wSaveUSI  = AMEusi
053700170227     c                   eval      Cod       = 'XH'
053800170227     c                   eval      Koy       = AMEusi
053900170227     c     kTAB          chain     TABEL
054000170227     c                   if        %found( TABEL00F )
054100170227     c                   eval      dsXH      = TBLuni
054200170227     c                   endif
054300170227     c                   endif
054400170227     c                   eval      WAMAusiD  = dsXH.§XHdes
054500170227e   1c                   EndIf
054600170227     c                   eval      WAMAcvf   = AMEcvf
054700170227     c                   eval      WAMApbl   = AMEpbl
054800170227     c                   eval      WAMAsta   = AMEsta
054900170227if  1c                   If        AMEsta   <> *blanks
055000170227     c                   if        AMEsta   <> wSaveSTA
055100170227     c                   clear                   dsXF
055200170227     c                   eval      wSaveSTA  = AMEsta
055300170227     c                   eval      Cod       = 'XF'
055400170227     c                   eval      Koy       = AMEsta
055500170227     c     kTAB          chain     TABEL
055600170227     c                   if        %found( TABEL00F )
055700170227     c                   eval      dsXF      = TBLuni
055800170227     c                   endif
055900170227     c                   endif
056000170227     c                   eval      WAMAstaD  = dsXF.§XFdes
056100170227e   1c                   EndIf
056200170227     c                   eval      WAMAcsp   = AMEcsp
056300170227if  1c                   If        AMEcsp   <> *blanks
056400170227     c                   if        AMEcsp   <> wSaveCSP
056500170227     c*//                clear                   dsXR
056600170227     c                   clear                   WdesXR
056700170227     c                   eval      wSaveCSP  = AMEcsp
056800170227     c                   eval      Cod       = 'XR'
056900170227     c                   eval      Koy       = AMEcsp
057000170227     c     kTAB          chain     TABEL
057100170227     c                   if        %found( TABEL00F )
057200170227     c*//                eval      dsXR      = TBLuni
057300170227     c                   eval      WdesXR    = TBLuni
057400170227     c                   endif
057500170227     c                   endif
057600170227     c                   eval      WAMAcspD  = wDesXR
057700170227e   1c                   EndIf
057800170227     c                   eval      WAMAdsp   =  AMEdsp
057900160831     c                   eval      WAMAIMT  =  AMEIMT
058000160831     c                   eval      WAMAIMP  =  AMEIMP
058100160831     c                   eval      WAMAIMA  =  AMEIMA
058200160831     c                   eval      WAMAIMF  =  AMEIMF
058300160831     c                   eval      WAMAIMK  =  AMEIMK
058400160831     c                   eval      WAMaDTC  =  AMEDTC
058500160831     c                   eval      WAMaDTP  =  AMEDTP
058600160831     c                   eval      WAMADRC  =  AMEDRC
058700160831     c                   eval      WAMADFI  =  AMEDFI
058800160831     c                   eval      WAMACRC  =  AMECRC
058900170227if  1c                   If        AMEcrc   <> *blanks
059000170227     c                   if        AMEcrc   <> wSaveCRC
059100170227     c                   clear                   dsXD
059200170227     c                   eval      wSaveCRC  = AMEcrc
059300170227     c                   eval      Cod       = 'XD'
059400170227     c                   eval      Koy       = AMEcrc
059500170227     c     kTAB          chain     TABEL
059600170227     c                   if        %found( TABEL00F )
059700170227     c                   eval      dsXD      = TBLuni
059800170227     c                   endif
059900170227     c                   endif
060000170227     c                   eval      WAMAcrcD  = dsXD.§XDdes
060100170227e   1c                   EndIf
060200160831     c                   eval      WAMACFI  =  AMECFI
060300170227if  1c                   If        AMEcfi   <> *blanks
060400170227     c                   if        AMEcfi   <> wSaveCFI
060500170227     c                   clear                   dsXD
060600170227     c                   eval      wSaveCFI  = AMEcfi
060700170227     c                   eval      Cod       = 'XD'
060800170227     c                   eval      Koy       = AMEcfi
060900170227     c     kTAB          chain     TABEL
061000170227     c                   if        %found( TABEL00F )
061100170227     c                   eval      dsXD      = TBLuni
061200170227     c                   endif
061300170227     c                   endif
061400170227     c                   eval      WAMAcfiD  = dsXD.§XDdes
061500170227e   1c                   EndIf
061600160831     c                   eval      WAMADUR  =  AMEDUR
061700160831     c                   eval      WAMATEL  =  AMETEL
061800160831     c                   eval      wamaute  =  knmus
061900160831     c                   eval      wamadat  =  w0140
062000160831     c                   write     WMZAMA0
062100160831     c                   endsr
062200170227      *
062300170227      *---------------------------------------------------------------*
062400170227      *?Reperimento Dati del job (Utente/Operativi)                  ?*
062500170227      *---------------------------------------------------------------*
062600170227     c     DatiJob       BegSr
062700170227      *
062800170227     c     *dtaara       define    §azute        azuteds
062900170227     c     *dtaara       define    §datiute      ddatiute
063000170227      *
063100170227     c                   in(E)     *dtaara
063200170227     c                   IF        %ERROR or RSUT = *blanks
063300170227     c                   clear                   Tibs34Ds
063400170227     c                   call      'TIBS34R'
063500170227     c                   parm                    Tibs34Ds
063600170227     c                   in        *dtaara
063700170227     c                   ENDIF
063800170227      *
063900170227     c                   EndSr
064000170227      *
064100011212     C**********************************************************************
064200011212     c     *inzsr        begsr
064300011212     C**********************************************************************
064400011212     C     *ENTRY        PLIST
064500011212     C                   PARM                    KPJBA
064600170222     C                   MOVEl     KPJBU         PARAM
064700170227      *
064800170227      * -?Reperimento dati job?
064900170227     c                   exsr      DatiJob
065000170222      *
065100170227     C                   Z-ADD     1             CODUT             1 0
065200170227     C                   MOVEL     RSUT          RSUT2            25
065300011212     C* LA DATA DEL GIORNO LA PRENDO DA TIME
065400011212     C                   TIME                    W0140            14 0
065500011212     C                   MOVE      W0140         UDATE8            8 0
065600011212     C*
065700170222if  1c                   If        VfileSt = 'F'
065800160901      *elimina file in qtemp x la filiale
065900160901     c                   eval      file = 'MZ' + knmus
066000160901      *
066100160901     C                   close     wmzama0f
066200160901     c                   movea     cmd2          coman
066300160901     c                   exsr      comandi
066400160901     C                   open      wmzama0f
066500170222e   1c                   EndIf
066600170222if  1c                   If        VfileSt = 'S'
066700170222     C                   open      QsysPrt
066800170222e   1c                   EndIf
066900170227      *
067000170227      * -?Impostazione campi fissi nelle key-list?
067100170227     c                   eval      ENTsocieta = ParamSOC
067200170227     c                   eval      ENTtpRifAn = 'CDC'
067300170227      *
067400170227      * -?Definizione key-list?
067500170227      * -?File ANENT01L?
067600170227     c     keyANENT01    klist
067700170227     c                   kfld                    ENTsocieta
067800170227     c                   kfld                    ENTtpRifAn
067900170227     c                   kfld                    ENTentita
068000170227      *
068100011212     C* POSIZIONAMENTO TABELLE                             *
068200011212     C     KTAB          KLIST                                                  *
068300011212     C                   KFLD                    CODUT                          *
068400011212     C                   KFLD                    COD               2            *
068500011212     C                   KFLD                    KOY               8            *
068600160517      *
068700160517     C     KTbe          KLIST                                                  *
068800160517     C                   KFLD                    tbecod                         *
068900160517     C                   KFLD                    tbeke1                         *
069000011212     C*
069100011212     C                   MOVEA     KEYFLD        KEY
069200011212     C                   Z-ADD     66            CONRIG            3 0
069300011212     C                   DO        5             I                 3 0
069400011212     C     KEY(I)        IFNE      'AMETGN'
069500011212     C                   Z-ADD     1             Y                 3 0
069600011212     C     KEY(I)        LOOKUP    CMP(Y)                                 21
069700011212     C   21              Z-ADD     I             SEL(Y)
069800011212     C                   END
069900011212     C                   END
070000160517     c                   if        vmau <> *blank
070100160517     c                   clear                   vmaud            30
070200160517     c                   movel     'MAU'         tbecod
070300160517     c                   movel(p)  vmau          tbeke1
070400160517     c     ktbe          chain     tntbe01l
070500160517     c                   if        %found(tntbe01l)
070600160517     c                   movel     tbeuni        dmau
070700160517     c                   movel     §maudes       vmaud
070800160517     c                   endif
070900160517     c                   endif
071000160517     C                   if        Vcaut <> *blank
071100160517     c                   movel     'CAU'         tbecod
071200160517     c                   movel(p)  vcaut         tbeke1
071300160517     c     ktbe          chain     tntbe01l
071400160517     c                   if        %found(tntbe01l)
071500160517     c                   movel     tbeuni        dcau
071600160517     c                   movel     §caudes       vcautd           30
071700160517     c                   endif
071800160517     c                   endif
071900160809      * verifico presenza date periodi selezione
072000160809     c                   clear                   vdadur            8 0
072100160809     c                   if        vdadug > 0
072200160809     C                   MOVE      Vdadug        G02inv
072300160809     C                   MOVEL     '3'           G02ERR
072400160809     C                   CALL      'XSRDA8'
072500160809     C                   PARM                    WLBDA8
072600160809     C     G02ERR        IFne      '1'
072700160809     c                   move      g02dat        vdadur
072800160809     C                   END
072900160809     c                   endif
073000160809     c                   clear                   valdur            8 0
073100160809     c                   if        valdug > 0
073200160809     C                   MOVE      Valdug        G02inv
073300160809     C                   MOVEL     '3'           G02ERR
073400160809     C                   CALL      'XSRDA8'
073500160809     C                   PARM                    WLBDA8
073600160809     C     G02ERR        IFne      '1'
073700160809     c                   move      g02dat        valdur
073800160809     C                   END
073900160809     c                   endif
074000160809     c                   clear                   vusodal           8 0
074100160809     c                   if        vusodg > 0
074200160809     C                   MOVE      Vusodg        G02inv
074300160809     C                   MOVEL     '3'           G02ERR
074400160809     C                   CALL      'XSRDA8'
074500160809     C                   PARM                    WLBDA8
074600160809     C     G02ERR        IFne      '1'
074700160809     c                   move      g02dat        vusodal
074800160809     C                   END
074900160809     c                   endif
075000160809     c                   clear                   vusoal            8 0
075100160809     c                   if        vusoag > 0
075200160809     C                   MOVE      Vusoag        G02inv
075300160809     C                   MOVEL     '3'           G02ERR
075400160809     C                   CALL      'XSRDA8'
075500160809     C                   PARM                    WLBDA8
075600160809     C     G02ERR        IFne      '1'
075700160809     c                   move      g02dat        vusoal
075800160809     C                   END
075900160809     c                   endif
076000011212     c* preparo istruzione sql
076100011212     c                   exsr      srsql
076200011212     C*
076300011212     C                   Z-ADD     VIMM          DATO
076400011212     C                   Z-ADD     MESE          APPO              2 0
076500011212     C                   Z-ADD     ANNO          MESE
076600011212     C                   Z-ADD     APPO          ANNO
076700011212     C                   Z-ADD     DATO          VIMM
076800011212     c                   endsr
076900000000     C*-----------------------------------------------------
077000000000     C* STAMPA SCELTE
077100000000     C*-----------------------------------------------------
077200000000     OQSYSPRT   E            TES1             01
077300000000     O                       S(1)                66
077400000000     O                       S(2)               132
077500000000     O                       RSUT                20
077600961029     O                       UDATE8             120 '  /  /    '
077700000000     O                       PAGE          Z    132
077800000000     O          E            TES1             02
077900000000     O                                           77 '  SELEZIONE STAMPE  '
078000011213     O          e            TES1             03
078100011213     O                       S(7)                66
078200011213     O                       S(8)               132
078300000000     O          E            TES1             07
078400000000     O                       SEL(2)        Z      2
078500000000     O                                           27 'TARGA ................'
078600000000     O                       VTGA                32
078700000000     O          E            TES1             09
078800000000     O                       SEL(3)        Z      2
078900000000     O                                           27 'IMMATRICOLAZIONE .....'
079000910219     O                       VIMM                35 '  /  '
079100000000     O          E            TES1             11
079200000000     O                       SEL(4)        Z      2
079300170222     O                                           27 'CATEGORIA ............'
079400910219     O                       VCAT                32
079500011213     O          E            TES1             13
079600011212     O                       SEL(5)        Z      2
079700000000     O                                           27 'MARCA ................'
079800160517     O                       Vcaut               +2
079900160517     O                       Vcautd              +1
080000011213     O          E            TES1             15
080100011212     O                       SEL(6)        Z      2
080200000000     O                                           27 'TIPO .................'
080300160517RE   O                       Vmau                +2
080400160517RE   O                       Vmaud               +1
080500011213     O          E            TES1             17
080600011212     O                       SEL(7)        Z      2
080700000000     O                                           27 'DITTA ................'
080800011212     O                       VDIT                33
080900011213     O          E            TES1             19
081000011212     O                       SEL(8)        Z      2
081100170222     O                                           27 'FILIALE ..............'
081200011212     O                       VFIL          z     32
081300011213     O          E            TES1             21
081400011212     O                       SEL(9)        Z      2
081500961029     O                                           27 'CENTRO DI COSTO ......'
081600891106     O                       VCDC                35
081700160809     O          E            TES1             23
081800160809     O                       SEL(10)       Z      2
081900170222     O                                           27 'ULTIMA REVISIONE DAL .'
082000160809     O                       VDADUR              38 '  /  /    '
082100160809     O                                         +  2 'AL'
082200160809     O                       VALDUR            +  2 '  /  /    '
082300160809     O          E            TES1             25
082400160809     O                       SEL(11)       Z      2
082500170222     O                                           27 'DATA IN USO DAL ......'
082600160809     O                       VUSODAL             38 '  /  /    '
082700160809     O                                         +  2 'AL'
082800160809     O                       VUSOAL            +  2 '  /  /    '
082900000000     C*-----------------------------------------------------
083000000000     C* STAMPA TESTATA
083100000000     C*-----------------------------------------------------
083200000000     O          E            TESTA            01
083300000000     O                       S(1)                66
083400000000     O                       S(2)               132
083500000000     O                       RSUT                20
083600961029     O                       UDATE8             120 '  /  /    '
083700000000     O                       PAGE          Z    132
083800000000     O          E            TESTA            02
083900000000     O                       S(3)                66
084000000000     O                       S(4)               132
084100011213     O          E            TESTA            03
084200011213     O                       S(7)                66
084300011213     O                       S(8)               132
084400011212     O          E            TESTA            05
084500011213     O                       S(5)                66
084600011213     O                       S(6)               132
084700011212     O          E            TESTA            06
084800011213     O                                           +0 'N.RIFER|N. TARGA    |MARCA'
084900011213     O                                           +0 '         TIPO             '
085000011213     O                                           +0 '   |CT|'
085100011213     O                                           +0 ' TARA  P.UTILE  P.P.CAR.'
085200011213     O                                           +0 '|DIT FIL'
085300011213     O                                           +0 '|TAS|    TASSA   |RCA|    '
085400011213     O                                           +0 'IMPORTO  |'
085500011213     O          E            TESTA            07
085600011213     O                                           +0 '       |            |     '
085700011213     O                                           +0 '         CILINDRATA       '
085800011213     O                                           +0 '   |  |'
085900011213     O                                           +0 'AUTISTA                 '
086000011213     O                                           +0 '|       '
086100011213     O                                           +0 '|PUB|CIRCOLAZIONE|I/F|    '
086200011213     O                                           +0 'KASCO    |'
086300011213     O          E            TESTA          1 09
086400011213     O                       S(5)                66
086500011213     O                       S(6)               132
086600000000     C*-----------------------------------------------------
086700000000     C* STAMPA DETTAGLIO
086800000000     C*-----------------------------------------------------
086900000000     O          E            DETAIL      1
087000000000     O                       AMENRF        Z      7
087100011213RE   O                       AMETGA              +1
087200011213RE   O                       AMETGN              +1
087300160809RE   O                       AMEcaut             +1
087400160517RE   O                       AMEcautd            +1
087500011213RE   O                       AMECAT              +1
087600011213RE   O              N50      AMETAR        Z     +3
087700011213RE   O              N50      AMEPQL        Z     +4
087800011213     O              N50      AMEPPC        Z     +6
087900011213     O               50      DESAUT              83
088000011213     O                       AMEDIT              +1
088100011213     O                       AMEFIL              +1
088200011213     O               16                          95 'NO'
088300011213     O              N16                          95 'SI'
088400011213     O                       AMEIMT        2     +1
088500011213     O                       AMECRC              +1
088600011213     O                       AMEIMK        2     +1
088700160517     O          E            DETAIL      1
088800160809     o                                              'Ult.Rev.'
088900160809     o                       amedurs       y     20
089000160517     O                       AMEmau              24
089100160517     O                       AMEmaud             +1
089200160517     O                       AMECIL              +2
089300160809     o                                              'Aut.Dal '
089400160809     o                       amedals       y     +1
089500011213     O                       AMECFI             112
089600000000      * SALTO RIGA
089700000000     O          E            XSAL        1
089800170222      *//**  CMD1 --------------------------------
089900170222      *//CLRPFM    FILE(WMZAMA0F)
090000170222      *//**  CMD6
090100170222      *//CPYF FROMFILE(WMZAMA0F) TOFILE(WRKQUERY/xxxxxxxxxx) MBROPT(*REPLACE)
090200000000**
090300000000*-DESDIT-----------****-------------------------------------------  S1
090400000000-------------------------------------***    XX/XX/XX      Pag. XXX  S2
090500000000                                         **  ELENCO  AUTOMEZZI  **  S3
090600011212                                                PGM.:   TRMZ04R1    S4
090700011213------------------------------------------------------------------  S5
090800011213---------------------------------------------------------------     S6
090900011213                    ***-------------------------------------------  S7
091000011213-------------------------------------***                            S8
091100000000**   CMP
091200000000AMENRF
091300000000AMETGA
091400000000AMEIMM
091500000000AMECAT
091600160809AMECAUT
091700160809AMEMAU
091800000000AMEDIT
091900000000AMEFIL
092000000000AMECDC
092100160809AMEDUR
092200160809AMEDAL
092300160901**  CMD2
092400160901CRTDUPOBJ OBJ(WMZAMA0F) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(QTEMP)
092500170222**  CMD3
092600170222DLTF FILE(WRKQUERY/xxxxxxxxxx)
092700160901**  CMD4
092800160901CHKOBJ  OBJ(WRKQUERY/xxxxxxxxxx) OBJTYPE(*FILE)
092900160901**  CMD5
093000160901CRTDUPOBJ OBJ(WMZAMA0F) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(WRKQUERY) NEWOBJ(xxxxxxxxxx) DATA(*YES)
