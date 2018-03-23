000100011213      *PARMS DFTACTGRP(*YES)
000200011213      *PARMS COMMIT(*NONE) DATFMT(*ISO)
000300011212      *PARMS CLOSQLCSR(*ENDMOD)
000400011212      *PARMS DYNUSRPRF(*OWNER)
000500000000     H DECEDIT('0,') DATEDIT(*DMY.)
000600000000     F*                   STAMPA AUTOMEZZI                 *
000700000000     F*----------------------------------------------------*
000800011212     FMZAME00F  IF   E           K DISK
000900910301     FTABEL00F  IF   E           K DISK
001000160517     FTntbe01l  IF   E           K DISK
001100160831     Fazorg01l  IF   E           K DISK
001200160831     Fwmzama0f  o    E             DISK
001300000000     FQSYSPRT   O    F  132        PRINTER
001400011213     D S               S             66    DIM(8) CTDATA PERRCD(1)              COSTANTI STAMPA
001500160809     D SEL             S              1  0 DIM(11)                              ORDINAMENTO
001600160809     D CMP             S              7    DIM(11) CTDATA PERRCD(1)             NOMI CAMPI
001700160809     D KEY             S              7    DIM(5)                               CAMPI PER KEYFLD
001800160901     d coman           s             99
001900160901     d luncmd          s             15  5
002000160901     d file            s             10
002100160901     D cmd1            S              1    DIM(99) CTDATA PERRCD(99)
002200160901     D cmd2            S              1    DIM(99) CTDATA PERRCD(99)
002300160901     D cmd3            S              1    DIM(99) CTDATA PERRCD(99)
002400160901     D cmd4            S              1    DIM(99) CTDATA PERRCD(99)
002500160901     D cmd5            S              1    DIM(99) CTDATA PERRCD(99)
002600160901     D cmd6            S              1    DIM(99) CTDATA PERRCD(99)
002700000000     D*----------------------------------------------------*
002800000000     D*
002900000000     D PARAM           DS
003000011212     D  SCAT                          1
003100011212     D  SDIT                          1
003200011212     D  SFIL                          1
003300160809     D  keyfld                       35
003400011213     D  ANNOV                         4  0
003500011213     D  MESEV                         2  0
003600011212     D  VTGA                          2
003700011212     D  VTGN                          9
003800011212     D  VIMM                          4  0
003900011212     D  VCAT                          2
004000160517     D  Vcaut                         2
004100160517     D  Vmau                          3
004200011212     D  VDIT                          3
004300011212     D  VFIL                          3  0
004400011212     D  VCDC                          8
004500160809     D  Vdadug                        8  0
004600160809     D  Valdug                        8  0
004700160809     D  Vusodg                        8  0
004800160809     D  Vusoag                        8  0
004900160517     D  dmau         E DS
005000160517     D  dcau         E DS
005100160831     D  dsxn         E DS
005200000000     D KPJBA         E DS
005300000000     D  LIBSYS                92    100
005400000000     D  MSGERR               453    502
005500910218     D                 DS
005600910218     D  DATO                   1      4  0
005700910218     D  MESE                   1      2  0
005800910218     D  ANNO                   3      4  0
005900000000     D UTEDSE        E DS                  EXTNAME(UTEDSE0F)
006000000000     D  TCU                  398    697
006100000000     D                                     DIM(50)                              TAB FLAG CNT
006200000000     D  KCU                  698    847P 0
006300000000     D                                     DIM(50)                              TAB CAPO CONTO
006400000000     D                                     PACKEVEN
006500000000     D  DTU                  848    895P 0
006600000000     D                                     DIM(12)                              DATE PARAM
006700000000     D                                     PACKEVEN
006800160809     D*-------------
006900160809     D WLBDA8          DS
007000160809     D  G02DAT                 1      8  0
007100160809     D  G02INV                 9     16  0
007200160809     D  G02ERR                17     17
007300160809     D  G02TGI                18     22  0
007400011212     D WrkSqlCmd       S           1024
007500011213     D mzame         E DS                  extname(mzame00f)
007600000000     I/SPACE 3
007700000000     C*---------------------------------------------------------------*
007800011212     c*
007900011212     c                   if        not *inlr
008000011212     c* Lettura WRKAME00F
008100011212     C/EXEC SQL
008200011212     C+ PREPARE S1 FROM :WrkSqlCmd
008300011212     C/END-EXEC
008400011212
008500011212     C/EXEC SQL
008600011212     C+ DECLARE A1 CURSOR FOR S1
008700011212     C/END-EXEC
008800011212
008900011212     C/EXEC SQL
009000011212     C+ OPEN A1
009100011212     C/END-EXEC
009200011212
009300011212     C                   DOU       SqlCod <> 0
009400011212     C/EXEC SQL
009500011212     C+ FETCH NEXT FROM A1 INTO :mzame
009600011212     C/END-EXEC
009700011212     c*
009800011212     C                   SELECT
009900011212     C                   WHEN      SqlCod = 0
010000011212     c* parametri
010100011212     c                   if        not *in99
010200011212     C                   EXCEPT    TES1
010300011212     C                   SETON                                            99
010400011212     c                   end
010500011212      *  STAMPA
010600011212     C   99              EXSR      XSALTO
010700011212     C  N99              SETON                                            99
010800011212     C                   EXSR      XSALVA
010900011212     C     AMEDTP        COMP      0                                      16
011000011212     C                   SETOFF                                       50
011100011212     C* DECODIFICA AUTISTA
011101170119     C                   clear                   DESAUT           24
011102170119     C                   clear                   funz              3
011200030715     C     AMECAT        IFEQ      'A1'
011300030715     C     AMECAT        orEQ      'A2'
011400030715     C     AMECAT        orEQ      'U1'
011500030715     C     AMECAT        orEQ      'U2'
011600011212     C                   MOVEL     'XN'          COD
011700011212     C                   MOVEL     *BLANKS       KOY
011800011212     C                   MOVEL     AMECAU        KOY
011900160831     C                   clear                   dsxn
012000160831     C     KTAB          CHAIN     TABEL00f                           22
012100160831     C                   if        %found(tabel00f)
012200160831     C                   MOVEL     TBLUNI        Dsxn
012300160831     c                   endif
012400160831     C                   MOVEL     §xndes        DESAUT           24
012500160831     C                   MOVEL     §xnfun        funz              3
012600011212     C                   SETON                                        50
012700011212     C                   END
012800011212     C*
012900011212     C     CONRIG        IFGT      60
013000011212     C                   EXCEPT    TESTA
013100011212     C                   Z-ADD     9             CONRIG
013200011212     C                   END
013300160517     c                   clear                   amemaud          30
013400160517     c                   movel     'MAU'         tbecod
013500160517     c                   movel(p)  amemau        tbeke1
013600160517     c     ktbe          chain     tntbe01l
013700160517     c                   if        %found(tntbe01l)
013800160517     c                   movel     tbeuni        dmau
013900160517     c                   movel     §maudes       amemaud
014000160517     c                   endif
014100160517     c                   clear                   amecautd         30
014200160517     c                   movel     'CAU'         tbecod
014300160809     c                   movel(p)  amecaut       tbeke1
014400160517     c     ktbe          chain     tntbe01l
014500160517     c                   if        %found(tntbe01l)
014600160517     c                   movel     tbeuni        dcau
014700160517     c                   movel     §caudes       amecautd
014800160517     c                   endif
014900160809     c                   clear                   amedurs           8 0
015000160809     c                   if        amedur > 0
015100160809     C                   MOVE      amedur        G02inv
015200160809     C                   MOVEL     '3'           G02ERR
015300160809     C                   CALL      'XSRDA8'
015400160809     C                   PARM                    WLBDA8
015500160809     C     G02ERR        IFne      '1'
015600160809     c                   move      g02dat        amedurs
015700160809     C                   END
015800160809     c                   endif
015900160809     c                   clear                   amedals           8 0
016000160809     c                   if        amedal > 0
016100160809     C                   MOVE      amedal        G02inv
016200160809     C                   MOVEL     '3'           G02ERR
016300160809     C                   CALL      'XSRDA8'
016400160809     C                   PARM                    WLBDA8
016500160809     C     G02ERR        IFne      '1'
016600160809     c                   move      g02dat        amedals
016700160809     C                   END
016800160809     c                   endif
016900160831     c                   exsr      scriviWF
017000160809      *
017100011212     C                   EXCEPT    DETAIL
017200011212     C                   ADD       3             CONRIG
017300011212     C                   WHEN      SqlCod = 100
017400011212     C                   WHEN      SqlCod <> 0
017500011212      * Forzo la stampa del JOBLOG.
017600011212     C                   CALL      'X66CHGJOB'
017700011212     C                   ENDSL
017800011212     C                   ENDDO
017900011212
018000011212     C/EXEC SQL
018100011212     C+ CLOSE A1
018200011212     C/END-EXEC
018300011212     c                   end
018400011212     c*
018500160901      * operazioni di fine duplica da file in qtemp a libreria utente
018600160901      *verifica se esiste file in libreria di wrk per utente
018700160901     c                   close     wmzama0f
018800160901      *
018900160901     c                   movea     cmd4          coman
019000160901     c                   eval      %subst(coman:22:10) = file
019100160901     c                   exsr      comandi
019200160901      *se trova il file esegue copy file altrimenti lo crea
019300160901     c                   if        *in34
019400160901     c                   movea     cmd5          coman
019500160901     c                   eval      %subst(coman:78:10) = file
019600160901     c                   exsr      comandi
019700160901     c                   else
019800160901     c                   movea     cmd6          coman
019900160901     c                   eval      %subst(coman:41:10) = file
020000160901     c                   exsr      comandi
020100160901     c                   endif
020200011212     C                   seton                                        lr
020300160901     c*-------------------------------------------------
020400160901     c     comandi       begsr
020500160901     c*-------------------------------------------------
020600160901      *
020700160901     C                   z-add     99            luncmd
020800160901     C                   CALL      'QCMDEXC'                            34
020900160901     C                   parm                    coman
021000160901     C                   parm                    luncmd
021100160901      *
021200160901     c                   endsr
021300000000     C***------------------------------------------------***
021400000000     CSR   XSALVA        BEGSR                                                  *
021500011212     C                   MOVE      AMECAT        SAVCAT            2
021600011212     C                   move      AMEDIT        SAVDIT            3
021700011212     C                   move      AMEFIL        SAVFIL            3 0
021800000000     C                   ENDSR
021900000000     C***------------------------------------------------***
022000000000     C***   GESTISCE SALTO PAGINA E RIGA A ROTTURA   ----***
022100000000     C***------------------------------------------------***
022200000000     CSR   XSALTO        BEGSR                                                  *
022300000000      *
022400011212     c                   do
022500000000     C     SAVCAT        IFNE      AMECAT
022600000000     C     SCAT          ANDEQ     'P'
022700000000     C                   Z-ADD     70            CONRIG
022800011212     C                   leave
022900000000     C                   END
023000000000     C*
023100000000     C     SAVDIT        IFNE      AMEDIT
023200000000     C     SDIT          ANDEQ     'P'
023300000000     C                   Z-ADD     70            CONRIG
023400011212     C                   leave
023500000000     C                   END
023600000000     C*
023700000000     C     SAVFIL        IFNE      AMEFIL
023800000000     C     SFIL          ANDEQ     'P'
023900000000     C                   Z-ADD     70            CONRIG
024000011212     C                   leave
024100000000     C                   END
024200000000     **
024300000000     C     SAVCAT        IFNE      AMECAT
024400000000     C     SCAT          ANDEQ     'R'
024500000000     C                   EXCEPT    XSAL
024600011212     C                   leave
024700000000     C                   END
024800000000     **
024900000000     C     SAVDIT        IFNE      AMEDIT
025000000000     C     SDIT          ANDEQ     'R'
025100000000     C                   EXCEPT    XSAL
025200011212     C                   leave
025300000000     C                   END
025400000000     **
025500000000     C     SAVFIL        IFNE      AMEFIL
025600000000     C     SFIL          ANDEQ     'R'
025700000000     C                   EXCEPT    XSAL
025800011212     C                   leave
025900000000     C                   END
026000011212     C                   ENDdo
026100011212     C                   ENDSR
026200011212     c**********************************************************************
026300011212     c     srsql         begsr
026400011212     c**********************************************************************
026500011212     C                   EVAL      WrkSqlCmd
026600011212     C                             =
026700011212     C                             'SELECT'
026800011212     C                             +
026900011212     C                             ' * FROM MZAME00F '
027000011212     C                             +
027100011212     C                             ' WHERE AMENRF <> 0'
027200011212     c*scelta filiale
027300011212     C                   IF        vfil <> 0
027400011212     C                   EVAL      WrkSqlCmd
027500011212     C                             =
027600011212     C                             %TRIMR(WrkSqlCmd)
027700011212     C                             +
027800011212     C                             ' AND AMEFIL = ' + %editc(vfil:'Z')
027900011212     C                   endif
028000011212     c*scelta ditta
028100011212     C                   IF        vdit <> *blanks
028200011212     C                   EVAL      WrkSqlCmd
028300011212     C                             =
028400011212     C                             %TRIMR(WrkSqlCmd)
028500011212     C                             +
028600011212     C                             ' AND AMEDIT = ' + '''' + VDIT + ''''
028700011212     C                   endif
028800011212     c*scelta cdc
028900011212     C                   IF        vcdc <> *blanks
029000011212     C                   EVAL      WrkSqlCmd
029100011212     C                             =
029200011212     C                             %TRIMR(WrkSqlCmd)
029300011212     C                             +
029400011212     C                             ' AND AMECDC = ' + '''' + Vcdc + ''''
029500011212     C                   endif
029600011212     c*scelta categoria
029700011212     C                   IF        vcat <> ' '
029800050526     c                   if        %subst(vcat: 2: 1) = '*'
029900050526     c                   movel     vcat          comcat            1
030000050526     C                   EVAL      WrkSqlCmd
030100050526     C                             =
030200050526     C                             %TRIMR(WrkSqlCmd)
030300050526     C                             +
030400050526     C                             ' AND substr(AMECAT, 1, 1) = '
030500050526     C                             + '''' + COMCAT + ''''
030600050526     c                   else
030700011212     C                   EVAL      WrkSqlCmd
030800011212     C                             =
030900011212     C                             %TRIMR(WrkSqlCmd)
031000011212     C                             +
031100011212     C                             ' AND AMECAT = ' + '''' + VCAT + ''''
031200011212     C                   endif
031300050526     C                   endif
031400011212     c*scelta immatricolazione
031500011212     C                   IF        vimm <> 0
031600011212     C                   EVAL      WrkSqlCmd
031700011212     C                             =
031800011212     C                             %TRIMR(WrkSqlCmd)
031900011212     C                             +
032000011212     C                             ' AND AMEIMM = ' + %editc(vimm:'Z')
032100011212     C                   endif
032200160809     c*scelta data ultima revisione
032300160809     C                   IF        vdadug <> 0
032400160809     C                   EVAL      WrkSqlCmd
032500160809     C                             =
032600160809     C                             %TRIMR(WrkSqlCmd)
032700160809     C                             +
032800160809     C                             ' AND AMEDUR between ' + %editc(vdadug:'Z')
032900160809     C                             + ' AND ' + %editc(valdug:'Z')
033000160809     C                   endif
033100160809     c*scelta data in uso dal ...
033200160809     C                   IF        vusodg <> 0
033300160809     C                   EVAL      WrkSqlCmd
033400160809     C                             =
033500160809     C                             %TRIMR(WrkSqlCmd)
033600160809     C                             +
033700160809     C                             ' AND AMEDAL between ' + %editc(vusodg:'Z')
033800160809     C                             + ' AND ' + %editc(vusoag:'Z')
033900160809     C                   endif
034000011212     c*scelta targa
034100011212     C                   IF        vtga <> ' '
034200011212     C                   EVAL      WrkSqlCmd
034300011212     C                             =
034400011212     C                             %TRIMR(WrkSqlCmd)
034500011212     C                             +
034600011212     C                             ' AND AMETGA = ' + '''' + VTGA + ''''
034700011212     C                             +
034800011212     C                             ' AND AMETGN = ' + '''' + VTGN + ''''
034900011212     C                   endif
035000020206     c*scelta marca
035100160517     C                   IF        vcaut <> ' '
035200020206     C                   EVAL      WrkSqlCmd
035300020206     C                             =
035400020206     C                             %TRIMR(WrkSqlCmd)
035500020206     C                             +
035600160809     C                             ' AND AMECAUT = ' + '''' + VCAUT + ''''
035700020206     C                   endif
035800011212     c*scelta marca e tipo
035900160517     C                   IF        vmau <> ' '
036000011212     C                   EVAL      WrkSqlCmd
036100011212     C                             =
036200011212     C                             %TRIMR(WrkSqlCmd)
036300011212     C                             +
036400160517     C                             ' AND AMEMAU = ' + '''' + VMAU + ''''
036500011212     C                   endif
036600020206     c*
036700011212     C                   EVAL      WrkSqlCmd
036800011212     C                             =
036900011212     C                             %TRIMR(WrkSqlCmd)
037000011212     C                             +
037100011212     C                             ' ORDER BY '
037200011212     c* ORDINAMENTO
037300011212     C                   EVAL      WrkSqlCmd
037400011212     C                             =
037500011212     C                             %TRIMR(WrkSqlCmd)
037600011212     C                             + ' ' + key(1)
037700011212     c     2             do        5             aa                2 0
037800011212     c                   if        key(AA) <> 'AMENRF'
037900011212     C                   EVAL      WrkSqlCmd
038000011212     C                             =
038100011212     C                             %TRIMR(WrkSqlCmd)
038200011212     c                             +
038300011212     C                             ',' + key(aa)
038400011212     c                   end
038500011212     c                   enddo
038600011212     c                   if        key(1) <> 'AMENRF'
038700011212     C                   EVAL      WrkSqlCmd
038800011212     C                             =
038900011212     C                             %TRIMR(WrkSqlCmd)
039000011212     C                             +
039100011212     C                             ', AMENRF'
039200011212     c                   end
039300011212     c*
039400011212     C                   ENDSR
039500160831     c*_____________________________________________________
039600160831     c     scriviWF      begsr
039700160831     c*_____________________________________________________
039800160831     c                   eval      WAMaNRF  =  AMENRF
039900160831     c                   eval      WAMaTGA  =  AMETGA
040000160831     c                   eval      WAMaTGN  =  AMETGN
040100160831     c                   eval      WAMaTLA  =  AMETLA
040200160831     c                   eval      WAMaCAT  =  AMECAT
040300160831     c                   eval      WAMaFIL  =  AMEFIL
040400160831     C     AMEFIL        CHAIN     AZORG                              10
040500160831     C                   MOVEL     *BLANKS       wamafild
040600160831     C  N10              MOVEL     ORGDES        wamafild
040700160831     c                   eval      WAMaCAU  =  AMECAU
040800160831     c                   eval      WAMamau  =  AMEmau
040900160831     c                   eval      WAMamaud =  AMEmaud
041000160831     c                   eval      WAMaCAUt =  AMECAUt
041100160831     c                   eval      WAMaCAUtd=  AMECAUtd
041200160831     c                   eval      WAMaCAUD =  desaut
041300160831     c                   eval      WAMafun =   funz
041400160831     c                   eval      WAMaDAL  =  AMEDAL
041500160831     c                   eval      WAMADIT  =  AMEDIT
041600160831     c                   eval      WAMAIMT  =  AMEIMT
041700160831     c                   eval      WAMAIMP  =  AMEIMP
041800160831     c                   eval      WAMAIMA  =  AMEIMA
041900160831     c                   eval      WAMAIMF  =  AMEIMF
042000160831     c                   eval      WAMAIMK  =  AMEIMK
042100160831     c                   eval      WAMaDTC  =  AMEDTC
042200160831     c                   eval      WAMaDTP  =  AMEDTP
042300160831     c                   eval      WAMADRC  =  AMEDRC
042400160831     c                   eval      WAMADFI  =  AMEDFI
042500160831     c                   eval      WAMACRC  =  AMECRC
042600160831     c                   eval      WAMACFI  =  AMECFI
042700160831     c                   eval      WAMADUR  =  AMEDUR
042800160831     c                   eval      WAMATEL  =  AMETEL
042900160831     c                   eval      wamaute  =  knmus
043000160831     c                   eval      wamadat  =  w0140
043100160831     c                   write     WMZAMA0
043200160831     c                   endsr
043300011212     C**********************************************************************
043400011212     c     *inzsr        begsr
043500011212     C**********************************************************************
043600011212     C     *ENTRY        PLIST
043700011212     C                   PARM                    KPJBA
043800011212     C                   Z-ADD     1             CODUT
043900011212     C                   CALL      'XPARUT'
044000011212     C                   PARM                    UTEDSE
044100011212     C     CODUT         ifeq      -1
044200011212     c                   seton                                        lr
044300011212     c                   end
044400011212     C                   MOVEL     RAGUT         RSUT             20
044500011212     C                   MOVEL     RAGUT         RSUT2            25
044600011212     C* LA DATA DEL GIORNO LA PRENDO DA TIME
044700011212     C                   TIME                    W0140            14 0
044800011212     C                   MOVE      W0140         UDATE8            8 0
044900011212     C*
045000160901      *elimina file in qtemp x la filiale
045100160901     c                   eval      file = 'MZ' + knmus
045200160901      *
045300160901     C                   close     wmzama0f
045400160901     c                   movea     cmd2          coman
045500160901     c                   exsr      comandi
045600160901     C                   open      wmzama0f
045700011212     C* POSIZIONAMENTO TABELLE                             *
045800011212     C     KTAB          KLIST                                                  *
045900011212     C                   KFLD                    CODUT                          *
046000011212     C                   KFLD                    COD               2            *
046100011212     C                   KFLD                    KOY               8            *
046200160517      *
046300160517     C     KTbe          KLIST                                                  *
046400160517     C                   KFLD                    tbecod                         *
046500160517     C                   KFLD                    tbeke1                         *
046600011212     C*
046700011212     C                   MOVEl     KPJBU         PARAM
046800011212     C                   MOVEA     KEYFLD        KEY
046900011212     C                   Z-ADD     66            CONRIG            3 0
047000011212     C                   DO        5             I                 3 0
047100011212     C     KEY(I)        IFNE      'AMETGN'
047200011212     C                   Z-ADD     1             Y                 3 0
047300011212     C     KEY(I)        LOOKUP    CMP(Y)                                 21
047400011212     C   21              Z-ADD     I             SEL(Y)
047500011212     C                   END
047600011212     C                   END
047700160517     c                   if        vmau <> *blank
047800160517     c                   clear                   vmaud            30
047900160517     c                   movel     'MAU'         tbecod
048000160517     c                   movel(p)  vmau          tbeke1
048100160517     c     ktbe          chain     tntbe01l
048200160517     c                   if        %found(tntbe01l)
048300160517     c                   movel     tbeuni        dmau
048400160517     c                   movel     §maudes       vmaud
048500160517     c                   endif
048600160517     c                   endif
048700160517     C                   if        Vcaut <> *blank
048800160517     c                   movel     'CAU'         tbecod
048900160517     c                   movel(p)  vcaut         tbeke1
049000160517     c     ktbe          chain     tntbe01l
049100160517     c                   if        %found(tntbe01l)
049200160517     c                   movel     tbeuni        dcau
049300160517     c                   movel     §caudes       vcautd           30
049400160517     c                   endif
049500160517     c                   endif
049600160809      * verifico presenza date periodi selezione
049700160809     c                   clear                   vdadur            8 0
049800160809     c                   if        vdadug > 0
049900160809     C                   MOVE      Vdadug        G02inv
050000160809     C                   MOVEL     '3'           G02ERR
050100160809     C                   CALL      'XSRDA8'
050200160809     C                   PARM                    WLBDA8
050300160809     C     G02ERR        IFne      '1'
050400160809     c                   move      g02dat        vdadur
050500160809     C                   END
050600160809     c                   endif
050700160809     c                   clear                   valdur            8 0
050800160809     c                   if        valdug > 0
050900160809     C                   MOVE      Valdug        G02inv
051000160809     C                   MOVEL     '3'           G02ERR
051100160809     C                   CALL      'XSRDA8'
051200160809     C                   PARM                    WLBDA8
051300160809     C     G02ERR        IFne      '1'
051400160809     c                   move      g02dat        valdur
051500160809     C                   END
051600160809     c                   endif
051700160809     c                   clear                   vusodal           8 0
051800160809     c                   if        vusodg > 0
051900160809     C                   MOVE      Vusodg        G02inv
052000160809     C                   MOVEL     '3'           G02ERR
052100160809     C                   CALL      'XSRDA8'
052200160809     C                   PARM                    WLBDA8
052300160809     C     G02ERR        IFne      '1'
052400160809     c                   move      g02dat        vusodal
052500160809     C                   END
052600160809     c                   endif
052700160809     c                   clear                   vusoal            8 0
052800160809     c                   if        vusoag > 0
052900160809     C                   MOVE      Vusoag        G02inv
053000160809     C                   MOVEL     '3'           G02ERR
053100160809     C                   CALL      'XSRDA8'
053200160809     C                   PARM                    WLBDA8
053300160809     C     G02ERR        IFne      '1'
053400160809     c                   move      g02dat        vusoal
053500160809     C                   END
053600160809     c                   endif
053700011212     c* preparo istruzione sql
053800011212     c                   exsr      srsql
053900011212     C*
054000011212     C                   Z-ADD     VIMM          DATO
054100011212     C                   Z-ADD     MESE          APPO              2 0
054200011212     C                   Z-ADD     ANNO          MESE
054300011212     C                   Z-ADD     APPO          ANNO
054400011212     C                   Z-ADD     DATO          VIMM
054500011212     c                   endsr
054600000000     C*-----------------------------------------------------
054700000000     C* STAMPA SCELTE
054800000000     C*-----------------------------------------------------
054900000000     OQSYSPRT   E            TES1             01
055000000000     O                       S(1)                66
055100000000     O                       S(2)               132
055200000000     O                       RSUT                20
055300961029     O                       UDATE8             120 '  /  /    '
055400000000     O                       PAGE          Z    132
055500000000     O          E            TES1             02
055600000000     O                                           77 '  SELEZIONE STAMPE  '
055700011213     O          e            TES1             03
055800011213     O                       S(7)                66
055900011213     O                       S(8)               132
056000000000     O          E            TES1             07
056100000000     O                       SEL(2)        Z      2
056200000000     O                                           27 'TARGA ................'
056300000000     O                       VTGA                32
056400000000     O          E            TES1             09
056500000000     O                       SEL(3)        Z      2
056600000000     O                                           27 'IMMATRICOLAZIONE .....'
056700910219     O                       VIMM                35 '  /  '
056800000000     O          E            TES1             11
056900000000     O                       SEL(4)        Z      2
057000000000     O                                           27 'CATEGORIA.............'
057100910219     O                       VCAT                32
057200011213     O          E            TES1             13
057300011212     O                       SEL(5)        Z      2
057400000000     O                                           27 'MARCA ................'
057500160517     O                       Vcaut               +2
057600160517     O                       Vcautd              +1
057700011213     O          E            TES1             15
057800011212     O                       SEL(6)        Z      2
057900000000     O                                           27 'TIPO .................'
058000160517RE   O                       Vmau                +2
058100160517RE   O                       Vmaud               +1
058200011213     O          E            TES1             17
058300011212     O                       SEL(7)        Z      2
058400000000     O                                           27 'DITTA ................'
058500011212     O                       VDIT                33
058600011213     O          E            TES1             19
058700011212     O                       SEL(8)        Z      2
058800961029     O                                           27 'PUNTO OPERATIVO ......'
058900011212     O                       VFIL          z     32
059000011213     O          E            TES1             21
059100011212     O                       SEL(9)        Z      2
059200961029     O                                           27 'CENTRO DI COSTO ......'
059300891106     O                       VCDC                35
059400160809     O          E            TES1             23
059500160809     O                       SEL(10)       Z      2
059600160809     O                                           27 'ULTIMA REVISIONE DAL..'
059700160809     O                       VDADUR              38 '  /  /    '
059800160809     O                                         +  2 'AL'
059900160809     O                       VALDUR            +  2 '  /  /    '
060000160809     O          E            TES1             25
060100160809     O                       SEL(11)       Z      2
060200160809     O                                           27 'DATA IN USO DAL.......'
060300160809     O                       VUSODAL             38 '  /  /    '
060400160809     O                                         +  2 'AL'
060500160809     O                       VUSOAL            +  2 '  /  /    '
060600000000     C*-----------------------------------------------------
060700000000     C* STAMPA TESTATA
060800000000     C*-----------------------------------------------------
060900000000     O          E            TESTA            01
061000000000     O                       S(1)                66
061100000000     O                       S(2)               132
061200000000     O                       RSUT                20
061300961029     O                       UDATE8             120 '  /  /    '
061400000000     O                       PAGE          Z    132
061500000000     O          E            TESTA            02
061600000000     O                       S(3)                66
061700000000     O                       S(4)               132
061800011213     O          E            TESTA            03
061900011213     O                       S(7)                66
062000011213     O                       S(8)               132
062100011212     O          E            TESTA            05
062200011213     O                       S(5)                66
062300011213     O                       S(6)               132
062400011212     O          E            TESTA            06
062500011213     O                                           +0 'N.RIFER|N. TARGA    |MARCA'
062600011213     O                                           +0 '         TIPO             '
062700011213     O                                           +0 '   |CT|'
062800011213     O                                           +0 ' TARA  P.UTILE  P.P.CAR.'
062900011213     O                                           +0 '|DIT FIL'
063000011213     O                                           +0 '|TAS|    TASSA   |RCA|    '
063100011213     O                                           +0 'IMPORTO  |'
063200011213     O          E            TESTA            07
063300011213     O                                           +0 '       |            |     '
063400011213     O                                           +0 '         CILINDRATA       '
063500011213     O                                           +0 '   |  |'
063600011213     O                                           +0 'AUTISTA                 '
063700011213     O                                           +0 '|       '
063800011213     O                                           +0 '|PUB|CIRCOLAZIONE|I/F|    '
063900011213     O                                           +0 'KASCO    |'
064000011213     O          E            TESTA          1 09
064100011213     O                       S(5)                66
064200011213     O                       S(6)               132
064300000000     C*-----------------------------------------------------
064400000000     C* STAMPA DETTAGLIO
064500000000     C*-----------------------------------------------------
064600000000     O          E            DETAIL      1
064700000000     O                       AMENRF        Z      7
064800011213RE   O                       AMETGA              +1
064900011213RE   O                       AMETGN              +1
065000160809RE   O                       AMEcaut             +1
065100160517RE   O                       AMEcautd            +1
065200011213RE   O                       AMECAT              +1
065300011213RE   O              N50      AMETAR        Z     +3
065400011213RE   O              N50      AMEPQL        Z     +4
065500011213     O              N50      AMEPPC        Z     +6
065600011213     O               50      DESAUT              83
065700011213     O                       AMEDIT              +1
065800011213     O                       AMEFIL              +1
065900011213     O               16                          95 'NO'
066000011213     O              N16                          95 'SI'
066100011213     O                       AMEIMT        2     +1
066200011213     O                       AMECRC              +1
066300011213     O                       AMEIMK        2     +1
066400160517     O          E            DETAIL      1
066500160809     o                                              'Ult.Rev.'
066600160809     o                       amedurs       y     20
066700160517     O                       AMEmau              24
066800160517     O                       AMEmaud             +1
066900160517     O                       AMECIL              +2
067000160809     o                                              'Aut.Dal '
067100160809     o                       amedals       y     +1
067200011213     O                       AMECFI             112
067300000000      * SALTO RIGA
067400000000     O          E            XSAL        1
067500000000**
067600000000*-DESDIT-----------****-------------------------------------------  S1
067700000000-------------------------------------***    XX/XX/XX      Pag. XXX  S2
067800000000                                         **  ELENCO  AUTOMEZZI  **  S3
067900011212                                                PGM.:   TRMZ04R1    S4
068000011213------------------------------------------------------------------  S5
068100011213---------------------------------------------------------------     S6
068200011213                    ***-------------------------------------------  S7
068300011213-------------------------------------***                            S8
068400000000**   CMP
068500000000AMENRF
068600000000AMETGA
068700000000AMEIMM
068800000000AMECAT
068900160809AMECAUT
069000160809AMEMAU
069100000000AMEDIT
069200000000AMEFIL
069300000000AMECDC
069400160809AMEDUR
069500160809AMEDAL
069600160901**  CMD1 --------------------------------
069700160901CLRPFM    FILE(WMZAMA0F)
069800160901**  CMD2
069900160901CRTDUPOBJ OBJ(WMZAMA0F) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(QTEMP)
070000160901**  CMD3
070100160901DLTF FILE(QTEMP/WMZAMA0F)
070200160901**  CMD4
070300160901CHKOBJ  OBJ(WRKQUERY/xxxxxxxxxx) OBJTYPE(*FILE)
070400160901**  CMD5
070500160901CRTDUPOBJ OBJ(WMZAMA0F) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(WRKQUERY) NEWOBJ(xxxxxxxxxx) DATA(*YES)
070600160901**  CMD6
070700160901CPYF FROMFILE(WMZAMA0F) TOFILE(WRKQUERY/xxxxxxxxxx) MBROPT(*REPLACE)
