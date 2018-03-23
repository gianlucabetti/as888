000100151027     /*END
000200151027     H DECEDIT('0,') DATEDIT(*DMY.)
000300151027     h option(*nodebugio)
000400151027     h alwnull(*inputonly)
000500151027?     *--------------------------------------------------------------*
000600151027      * TNSC41R8                                                     *
000700151027      *       *-----------------------------------------*            *
000800151027      *          INTERROGAZIONE CONTRASSEGNI X SEDE                  *
000900151027      *          SELEZIONE DATI DI PAGAMENTO: ID BONIF.              *
001000151027      *       *-----------------------------------------*            *
001100151027      *                                                              *
001200151027?     *--------------------------------------------------------------*
001300151027      * KC -FINE LAVORO                                              *
001400151027      * KG -INTERROGAZIONE BOLLE                                     *
001500151027      * KI -SFLDROP                                                  *
001600151027      * KL -RITORNO                                                  *
001700151027?     *--------------------------------------------------------------*
001800151027     FTNCSA01L  IF   E           K DISK
001900151027     FTNCSM03L  IF   E           K DISK
002000151027     FTNSC41D   CF   E             WORKSTN
002100151027     F                                     SFILE(SC41S01:NRR)
002200151027?     *--------------------------------------------------------------*
002300151027?     *  DS                                                          *
002400151027?     *--------------------------------------------------------------*
002500151027     D KPJBA         E DS
002600151027      *
002700151027     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
002800151027     D CNCR80        E DS
002900151027     D*
003000151027     D TNSC40ds      E DS                  EXTNAME(TNSC40DS) prefix(£)
003100151027     D TNSC40        E DS                  EXTNAME(TNSC40DS)
003200151027     D TIBS69        E DS                  EXTNAME(TIBS69DS)
003300151027     D CNACO         E DS                  EXTNAME(CNACO00F)
003400151027     D CNIND         E DS                  EXTNAME(CNIND00F)
003500151027     D CNCLP         E DS                  EXTNAME(CNCLP00F)
003600151027     D FNCLS         E DS                  EXTNAME(FNCLS00F)
003700151027     D TNSB50        E DS                  EXTNAME(TNSB50DS)
003800151027      *
003900151027     d ds_TNCSB      e ds                  extname(TNCSB00F)
004000151027     d                                     inz
004100151027     ***D*
004200151027     ***D YEurCoDS      E DS
004300151027     ***D                                     INZ
004400151027     D*
004500151027     D WLBDA8          DS
004600151027     D  G02DAT                 1      8  0
004700151027     D  G02INV                 9     16  0
004800151027     D  G02ERR                17     17
004900151027     D  G02TGI                18     22  0
005000151027     D*
005100151027     D                SDS
005200151027     D  NOMPG                  1     10
005300151027?     *--------------------------------------------------------------*
005400151027?     *  VARIABILI                                                   *
005500151027?     *--------------------------------------------------------------*
005600151027     d mnra            s                   like(csmnra)
005700151027     d mabi            s                   like(csmabi)
005800151027     d mcab            s                   like(csmcab)
005900151027      *
006000151027      * -?Stringhe SQL da eseguire?
006100151027     d wSQL            s          32740    inz   varying
006200151027?     *--------------------------------------------------------------*
006300151027?     *  COSTANTI TIPO DOCUMENTO                                     *
006400151027?     *--------------------------------------------------------------*
006500151027     D COST1           C                   CONST('BONIFICO  ')
006600151027     D COST2           C                   CONST('ASSEGNO   ')
006700151027     D COST3           C                   CONST('LETT.MITT.')
006800151027      *---------------------------------------------------------------*
006900151027      *   C I C L O      P R I N C I P A L E                          *
007000151027      *---------------------------------------------------------------*
007100151027     C*  CARICAMENTO SUBFILE
007200151027     C                   EXSR      CARSFL
007300151027     C*
007400151027     C*  LOOP
007500151027     C     WFINE         DOUEQ     'S'
007600151027     C*
007700151027     C     WTPVID        CASEQ     '1'           GESVD1
007800151027     C     WTPVID        CASEQ     '2'           GESVD2
007900151027     C                   ENDCS
008000151027     C*
008100151027     C                   ENDDO
008200151027     C*
008300151027     C*  OPERAZIONI FINALI / USCITA DAL *PGM
008400151027     C                   exsr      sr_RoutEnd
008500151027      *---------------------------------------------------------------*
008600151027      *  CARSFL: CARICAMENTO DATI DA SUBFILE                          *
008700151027      *---------------------------------------------------------------*
008800151027     C     CARSFL        BEGSR
008900151027     C*
009000151027     C                   SETON                                        LR
009100151027     C*
009200151027     C*  Pulisco subfile
009300151027     C                   SETOFF                                       2021
009400151027     C                   WRITE     SC41C01
009500151027     C                   SETON                                        21
009600151027      *
009700151027     C                   Z-ADD     0             NRR               4 0
009800151027     C                   MOVEL     '1'           WTPVID            1
009900151027     C*
010000151027     C*  Imposto variabili testata
010100151027     C                   eval      *in06  = *on
010200151027     C                   eval      V1Cnbn = D40nbn
010300151027     C*
010400151027     C                   MOVEL     NOMPG         NOMPGM
010500151027     C*
010600151027     C*  Caricamento subfile -----------------------------------------*
010700151027     C*
010800151027     c                   exsr      sr_PrepSQL
010900151027     c                   exsr      sr_ReadCursor
011000151027     C*
011100151027     C     *in23         IFEQ      '0'
011200151027     C                   EXSR      ROLLUP
011300151027     C                   ENDIF
011400151027     C*
011500151027     C     NRR           IFNE      0
011600151027     C                   MOVEL     '2'           WTPVID            1
011700151027     C                   ENDIF
011800151027     C*
011900151027     C                   ENDSR
012000151027      *
012100151027      *---------------------------------------------------------------*
012200151027      *?Preparazione stringa SQL per estrazione dati da TNCSB00F.    ?*
012300151027      *---------------------------------------------------------------*
012400151027     c     sr_PrepSQL    BEGSR
012500151027      *
012600151027      * -?Impostazione opzioni per SQL?
012700151027     c/exec sql
012800151027     c+     set  option  DynUsrPrf = *Owner,
012900151027     c+                  CloSqlCsr = *EndMod
013000151027     c/end-exec
013100151027      *
013200151027     c                   eval      wSQL = 'select TNCSB00F.* +
013300151028     c                                       from YHBOS00F inner join +
013400151028     c                                            TNCSB00F +
013500151027     c                                         on CSBaas = BONaas +
013600151027     c                                        and CSBlnp = BONlnp +
013700151027     c                                        and CSBnrs = BONnrs +
013800151027     c                                        and CSBnsp = BONnsp +
013900151027     c                                        and CSBtbl = BONtbl +
014000151027     c                                      where BONnbn = ' +
014100151027     c                                    %trim( %editc( D40nbn : '3' ) )
014200151027     c                                  + ' order by CSBaas, CSBlnp, +
014300151027     c                                               CSBnrs, CSBnsp'
014400151027      *
014500151027      * -?Dichiarazione cursore?
014600151027     c/exec sql
014700151027     c+     prepare S1   from :wSQL
014800151027     c/end-exec
014900151027     c/exec sql
015000151027     c+     declare C1   cursor for S1
015100151027     c/end-exec
015200151027      *
015300151027     c                   if        SQLcode < *zero
015400151027     c                   eval      *in23 = *on
015500151027     c                   exsr      sr_PrintERR
015600151027     c                   endif
015700151027      *
015800151027      * -?Apertura del cursore?
015900151027     c/exec sql
016000151027     c+     open C1
016100151027     c/end-exec
016200151027      *
016300151027     c                   if        SQLcode < *zero
016400151027     c                   eval      *in23 = *on
016500151027     c                   exsr      sr_CloseCursor
016600151027     c                   exsr      sr_PrintERR
016700151027     c                   endif
016800151027      *
016900151027     c                   ENDSR
017000151027      *
017100151027      *---------------------------------------------------------------*
017200151027      *?Lettura cursore.                                             ?*
017300151027      *---------------------------------------------------------------*
017400151027     c     sr_ReadCursor BEGSR
017500151027      *
017600151027     c                   eval      *in23 = *off
017700151027     c                   clear                   ds_TNCSB
017800151027      *
017900151027     c/exec SQL
018000151027     c+     fetch next   from C1   into :ds_TNCSB
018100151027     c/end-exec
018200151027      *
018300151027     c                   select
018400151027     c                   when      SQLcode = 100
018500151027     c                   eval      *in23 = *on
018600151027     c                   exsr      sr_CloseCursor
018700151027     c                   when      SQLcode < *zero
018800151027     c                   eval      *in23 = *on
018900151027     c                   exsr      sr_CloseCursor
019000151027     c                   exsr      sr_PrintERR
019100151027     c                   endsl
019200151027      *
019300151027     c                   ENDSR
019400151027      *
019500151027      *---------------------------------------------------------------*
019600151027      *?Chiusura cursore.                                            ?*
019700151027      *---------------------------------------------------------------*
019800151027     c     sr_CloseCursorBEGSR
019900151027      *
020000151027      * -?Chiusura del cursore?
020100151027     c/exec sql
020200151027     c+     close C1
020300151027     c/end-exec
020400151027      *
020500151027     c                   ENDSR
020600151027      *
020700151027      *---------------------------------------------------------------*
020800151027      *?Stampa segnalazione dell'errore rilevato via SQL             ?*
020900151027      *---------------------------------------------------------------*
021000151027     c     sr_PrintERR   BEGSR
021100151027      *
021200151027      * -?Stampa del Dump?
021300151027     c                   Dump(A)
021400151027      *
021500151027      * -?Stampa del Job-Log?
021600151027     c                   eval      Qcmd = 'DSPJOBLOG job(*) output(*print)'
021700151027     c                   eval      Qlen = %len( %trim( Qcmd ) )
021800151027     c                   call      'QCMDEXC'
021900151027     c                   parm                    Qcmd             35
022000151027     c                   parm                    Qlen             15 5
022100151027      *
022200151027      * -?Chiusura del programma?
022300151027     c                   exsr      sr_RoutEnd
022400151027      *
022500151027     c                   ENDSR
022600151027     C*----------------------------------------------------*
022700151027     C*   ROLLUP: Caricamento di una pagina                *
022800151027     C*----------------------------------------------------*
022900151027     C     ROLLUP        BEGSR
023000151027     C*
023100151027if  1C     *in23         IFEQ      '0'
023200151027     C                   Z-ADD     WSVRE1        NRR
023300151027     C                   Z-ADD     0             NRIG              4 0
023400151027     C*
023500151027do  2C     NRIG          DOWLT     16
023600151027     C     *in23         ANDEQ     '0'
023700151027     c                   EXSR      GESREC
023800151027     c                   exsr      sr_ReadCursor
023900151027     C                   ENDDO
024000151027     C*
024100151027     C     NRR           IFGT      WSVRE1
024200151027     C     WSVRE1        ADD       1             DSPNR1
024300151027     C                   Z-ADD     NRR           WSVRE1            4 0
024400151027     C                   ENDIF
024500151027     C*
024600151027e   1C                   ENDIF
024700151027     C*
024800151027     C                   ENDSR
024900151027     C*----------------------------------------------------*
025000151027     C*   srmulti - per multiassegni
025100151027     C*----------------------------------------------------*
025200151027     C     srmulti       BEGSR
025300151027     C*
025400151027     c                   eval      mnra = *hival
025500151027     c                   eval      mabi = *hival
025600151027     c                   eval      mcab = *hival
025700151027     c     csbnra        setll     tncsm03l
025800151027     c                   do        *hival
025900151027     c     csbnra        reade     tncsm03l
026000151027     c                   if        %eof(tncsm03l)
026100151027     c                   leave
026200151027     c                   endif
026300151027     C                   if        csmnra = WNRA
026400151027     c                   eval      mnra = csmnra
026500151027     c                   eval      mabi = csmabi
026600151027     c                   eval      mcab = csmcab
026700151027     c                   leave
026800151027     c                   endif
026900151027     c                   enddo
027000151027     C*
027100151027     C                   ENDSR
027200151027     C*----------------------------------------------------*
027300151027     C*   GESREC: Scrittura singolo record nel subfile     *
027400151027     C*----------------------------------------------------*
027500151027     C     GESREC        BEGSR
027600151027     C*
027700151027     C* Se il documento è valido controllo se il record rientra
027800151027     C* nei limiti richiesti
027900151027     C                   MOVEL     D40NAS        WNRA             10
028000151027if  1c                   if        %subst(csbnra: 10: 1)<>' ' and
028100151027     c                             csbtpi = 'M' and csbabi = 0
028200151027     c                   exsr      srmulti
028300151027x   1c                   else
028400151027     c                   eval      mnra = csbnra
028500151027     c                   eval      mabi = csbabi
028600151027     c                   eval      mcab = csbcab
028700151027e   1c                   endif
028800151027     C*----------------------------------------------------*
028900151027if  1C     D40ABI        IFEQ      *ZEROS
029000151027     C     D40ABI        OREQ      mABI
029100151027if  2C     D40CAB        IFEQ      *ZEROS
029200151027     C     D40CAB        OREQ      mCAB
029300151027if  3C     D40imp        IFEQ      *ZEROS
029400151027     C     D40imp        OREQ      csbcas
029500151027if  4C     WNRA          IFEQ      *BLANKS
029600151027     C     WNRA          OREQ      *ZEROS
029700151027     C     WNRA          OREQ      mNRA
029800151027     C*----------------------------------------------------*
029900151027     C* Selezione per stato C/Assegno
030000151027if  5C     D40TCR        IFEQ      ' '
030100151027     C     D40TCR        OREQ      'D'
030200151027     C     CSBDDC        ANDEQ     0
030300151027     C     D40TCR        OREQ      'G'
030400151027     C     CSBSTA        ANDEQ     2
030500151027     C     D40TCR        OREQ      'T'
030600151027     C     CSBSTA        ANDEQ     6
030700151027     C     D40TCR        OREQ      'R'
030800151027     C     CSBSTA        ANDEQ     7
030900151027     C     D40TCR        OREQ      'O'
031000151027     C     CSBSTA        ANDEQ     8
031100151027     C     D40TCR        OREQ      'A'
031200151027     C     CSBSTA        ANDEQ     9
031300151027     C     D40TCR        OREQ      'S'
031400151027     C     CSBSTA        ANDEQ     5
031500151027     C     D40TCR        OREQ      '3'
031600151027     C     CSBSTA        ANDEQ     3
031700151027     C     D40TCR        OREQ      'I'
031800151027     C     CSBDDC        ANDGT     0
031900151027     C     CSBDDP        ANDEQ     0
032000151027     C     D40TCR        OREQ      'P'
032100151027     C     CSBDDP        ANDGT     0
032200151027     C                   MOVEL     *BLANKS       V1CSCE
032300151027     C                   MOVEL     CSBAAS        V1CAAS
032400151027     C                   MOVEL     CSBMGS        MM                2 0
032500151027     C                   MOVE      CSBMGS        GG                2 0
032600151027     C                   MOVEL     GG            V1CDSP
032700151027     C                   MOVE      MM            V1CDSP
032800151027     C                   MOVE      CSBLNP        V1CLNP
032900151027     C                   MOVE      CSBNRS        V1CNRS
033000151027     C                   MOVE      CSBNSP        V1CNSP
033100151027     C                   MOVE      CSBTBL        V1CTBL
033200151027     C                   MOVEL     CSBRSD        V1CDES
033300151027     C                   MOVE      CSBRMN        V1CRMN
033400151027     C                   MOVE      CSBVCA        V1CVCA
033500151027     C                   Z-ADD     CSBCAS        V1CIMP
033600151027     C* Se lnp no del mio ramo imposto dati cliente infragr. (mitt.)
033700151027     C                   MOVE      CSBCDI        W8899             4 0
033800151027if  6C     W8899         IFEQ      8888
033900151027     C     W8899         OREQ      9999
034000151027     C                   Z-ADD     CSBAAS        KAAS
034100151027     C                   Z-ADD     CSBLNP        KLNP
034200151027     C                   Z-ADD     CSBNRS        KNRS
034300151027     C                   Z-ADD     CSBNSP        KNSP
034400151027     C                   MOVEL     *BLANKS       CSARSD
034500151027     C     KCSA          CHAIN     TNCSA01L                           31
034600151027     C                   MOVEL     CSARSD        V1CMIT
034700151027x   6C                   ELSE
034800151027     C                   MOVEL     *BLANKS       ACORAG
034900151027      * CHIAMO TIBS69 PER RICERCA CODICE CLIENTE CODIFICATO
035000151027     C                   CLEAR                   TIBS69
035100151027     C                   CLEAR                   CNACO
035200151027     C                   CLEAR                   CNIND
035300151027     C                   CLEAR                   CNCLP
035400151027     C                   CLEAR                   FNCLS
035500151027     C                   MOVEL     ' '           I69TLA
035600151027     C                   MOVEL     KNSIF         I69SIF
035700151027     C                   Z-ADD     CSBCDI        I69KAC
035800151027     C                   CALL      'TIBS69R'
035900151027     C                   PARM                    TIBS69
036000151027     C                   PARM                    CNACO
036100151027     C                   PARM                    CNIND
036200151027     C                   PARM                    CNCLP
036300151027     C                   PARM                    FNCLS
036400151027if  7C     O69ERR        IFEQ      *BLANK
036500151027     C                   MOVEL     ACORAG        V1CMIT
036600151027e   7C                   ENDIF
036700151027e   6C                   ENDIF
036800151027     C*-------------------------------------------------------*
036900151027     C                   ADD       1             NRR
037000151027     C                   ADD       1             NRIG
037100151027     C                   WRITE     SC41S01
037200151027     C*-------------------------------------------------------*
037300151027e   5C                   ENDIF
037400151027e   4C                   ENDIF
037500151027e   3C                   ENDIF
037600151027e   2C                   ENDIF
037700151027e   1C                   ENDIF
037800151027     C*
037900151027     C                   ENDSR
038000151027     C*----------------------------------------------------*
038100151027     C*  Routine gestione operazioni prima videata         *
038200151027     C*----------------------------------------------------*
038300151027     C     GESVD1        BEGSR
038400151027     C*-                                                   *
038500151027     C                   SETOFF                                       20
038600151027     C                   WRITE     SC41Z02
038700151027     C                   WRITE     SC41D02
038800151027     C                   EXFMT     SC41C01
038900151027     C*                                                    *
039000151027     C* Fine Programma                                     *
039100151027     C     *INKL         IFEQ      '1'
039200151027     C                   MOVEL     'S'           WFINE
039300151027     C                   ENDIF
039400151027     C*-                                                   *
039500151027     C                   ENDSR
039600151027     C*----------------------------------------------------*
039700151027     C*  Routine gestione operazioni subfile               *
039800151027     C*----------------------------------------------------*
039900151027     C     GESVD2        BEGSR
040000151027     C*-                                                   *
040100151027     C                   SETON                                        20
040200151027     C                   WRITE     SC41Z01
040300151027     C                   EXFMT     SC41C01
040400151027     C                   MOVEL     'N'           WINTBL            1
040500151027     C*                                                    *
040600151027     C* F3=Fine Programma                                     *
040700151027     C     *INKC         IFEQ      '1'
040800151027     C                   MOVEL     'S'           WFINE
040900151027     C                   MOVE      3             D40CMD
041000151027     C                   GOTO      FINVD2
041100151027     C                   ENDIF
041200151027     C* F12=Ritorno                                            *
041300151027     C     *INKL         IFEQ      '1'
041400151027     C                   MOVEL     'S'           WFINE
041500151027     C                   MOVE      12            D40CMD
041600151027     C                   GOTO      FINVD2
041700151027     C                   ENDIF
041800151027     C* ROLLUP
041900151027     C     *IN25         IFEQ      '1'
042000151027     C                   EXSR      ROLLUP
042100151027     C                   GOTO      FINVD2
042200151027     C                   ENDIF
042300151027      *
042400151027     C                   CLEAR                   TNSB50
042500151027     C                   EXSR      CNTVD2
042600151027     C* F7=Interrogazione Bolle                            *
042700151027     C     *INKG         IFEQ      '1'
042800151027     C     WINTBL        ANDEQ     'N'
042900151027     C                   EXSR      GESINT
043000151027     C                   ENDIF
043100151027     C*                                                    *
043200151027     C     FINVD2        ENDSR
043300151027     C*----------------------------------------------------*
043400151027     C*   GESINT: gestione interrogazione bolle            *
043500151027     C*----------------------------------------------------*
043600151027     C     GESINT        BEGSR
043700151027     C*                                                    *
043800151027     C                   CLEAR                   TNSB50
043900151027     C                   MOVEL     'C05'         I50OP0
044000151027     C                   MOVEL     '0'           O50F03
044100151027     C                   MOVEL     '0'           O50F12
044200151027     C                   MOVEL     '0'           O50ERR
044300151027     C                   MOVEL     *BLANKS       O50MSG
044400151027     C                   MOVEL     KPJBU         WSVKPJ          256
044500151027     C                   MOVEL     *BLANKS       KPJBU
044600151027     C     WACC          IFEQ      '1'
044700151027     C                   Z-ADD     V1CAAS        D50AAS
044800151027     C                   Z-ADD     V1CLNP        D50LNP
044900151027     C                   Z-ADD     V1CNRS        D50NRS
045000151027     C                   Z-ADD     V1CNSP        D50NSP
045100151027     C                   MOVEL     V1CTBL        D50TBL
045200151027     C                   MOVEL     'S'           WINTBL
045300151027     C                   MOVEL     TNSB50        KPJBU
045400151027     C                   CALL      'TNSB50R'
045500151027     C                   PARM                    KPJBA
045600151027     C                   MOVEL     KPJBU         TNSB50
045700151027     C     O50ERR        IFEQ      *BLANKS
045800151027     C                   GOTO      FININT
045900151027     C                   END
046000151027     C* Se ho richiamato il pgm con il numero spedizione/
046100151027     C* codice bolla prova a richiamare senza codice bolla
046200151027     C* come del resto faccio se non ho recuperato il C/Ass.
046300151027     C                   MOVEL     'C00'         I50OP0
046400151027     C                   MOVEL     V1CTBL        D50TBL
046500151027     C                   MOVEL     TNSB50        KPJBU
046600151027     C                   CALL      'TNSB50R'
046700151027     C                   PARM                    KPJBA
046800151027     C                   ELSE
046900151027     C                   MOVEL     TNSB50        KPJBU
047000151027     C                   CALL      'TNSB50R'
047100151027     C                   PARM                    KPJBA
047200151027     C                   END
047300151027     C                   MOVEL     KPJBU         TNSB50
047400151027     C                   MOVEL     WSVKPJ        KPJBU
047500151027     C*
047600151027     C     FININT        ENDSR
047700151027     C*----------------------------------------------------*
047800151027     C*   CNTVD2: Controlli subfile                        *
047900151027     C*----------------------------------------------------*
048000151027     C     CNTVD2        BEGSR
048100151027     C*                                                    *
048200151027     C*//                clear                   WACC
048300151027     C                   Z-ADD     1             NRR
048400151027     C                   MOVEL     ' '           V1CSCE
048500151027     C                   READC     SC41S01                                82
048600151027     C*                                                    *
048700151027do  1C     *IN82         DOWEQ     '0'
048800151027     C*                                                    *
048900151027if  2C     V1CSCE        IFEQ      '1'
049000151027     C                   MOVEL     *BLANKS       KPJBU
049100151027     C                   Z-ADD     NRR           SAVREC            4 0
049200151027     C*  F7=Interrogazione bolla
049300151027if  3C     *INKG         IFEQ      '1'
049400151027     C                   MOVEL     '1'           WACC              1
049500151027     C                   EXSR      GESINT
049600151027x   3C                   ELSE
049700151027     c                   eval      tnsc40ds = tnsc40
049800151027     C                   MOVE      V1CAAS        £D40AAS
049900151027     C                   MOVE      V1CLNP        £D40LNP
050000151027     C                   MOVE      V1CNRS        £D40NRS
050100151027     C                   MOVE      V1CNSP        £D40NSP
050200151027     C                   clear                   £D40ABI
050300151027     C                   clear                   £D40CAB
050400151027     C                   clear                   £D40NAS
050500151027     C                   MOVEL     TNSC40ds      KPJBU
050600151027     C* Interrogazione                                     *
050700151027     C                   CALL      'TNSC43R'
050800151027     C                   PARM                    KPJBA
050900151027e   3C                   ENDIF
051000151027     C*
051100151027     C                   Z-ADD     SAVREC        NRR
051200151027     C                   Z-ADD     SAVREC        DSPNR1
051300151027     C     NRR           CHAIN     SC41S01                            90
051400151027     C                   SETON                                        70
051500151027     C                   MOVEL     ' '           V1CSCE
051600151027     C  N90              UPDATE    SC41S01
051700151027e   2C                   ENDIF
051800151027     C*
051900151027     C                   READC     SC41S01                                82
052000151027e   1C                   ENDDO
052100151027     C*
052200151027     C                   ENDSR
052300151027      *---------------------------------------------------------------*
052400151027      *  *INZSR: OPERAZIONI INIZIALI                                  *
052500151027      *---------------------------------------------------------------*
052600151027     C     *INZSR        BEGSR
052700151027      *---------------------------------------------------------------*
052800151027     C     *ENTRY        PLIST
052900151027     C                   PARM                    KPJBA
053000151027     C                   MOVEL     KPJBU         TNSC40
053100151027     C*
053200151027     C                   Z-ADD     1             CODUT
053300151027     C                   CALL      'X§PARUT'
053400151027     C                   PARM                    UT§DSE
053500151027     C                   MOVEL     RAGUT         RSUT             20
053600151027     C                   MOVEL     REC80         CNCR80
053700151027     C                   MOVEL     KNMUS         §SEI              6
053800151027     C*----------------------------------------------------*
053900151027     C*  Definizione CHIAVI
054000151027     C     KCSA          KLIST
054100151027     C                   KFLD                    KAAS
054200151027     C                   KFLD                    KLNP
054300151027     C                   KFLD                    KNRS
054400151027     C                   KFLD                    KNSP
054500151027     C*----------------------------------------------------*
054600151027     C*  Definizione variabili
054700151027     C     *LIKE         DEFINE    CSBAAS        KAAS
054800151027     C     *LIKE         DEFINE    CSBLNP        KLNP
054900151027     C     *LIKE         DEFINE    CSBNRS        KNRS
055000151027     C     *LIKE         DEFINE    CSBNSP        KNSP
055100151027     C*----------------------------------------------------*
055200151027     C*  Inizializzazione delle variabii
055300151027     C                   MOVEL     'N'           WFINE             1
055400151027     C*----------------------------------------------------*
055500151027     C                   ENDSR
055600151027      *
055700151027      *---------------------------------------------------------------*
055800151027      *?Operazioni Finali.                                           ?*
055900151027      *---------------------------------------------------------------*
056000151027     c     sr_RoutEnd    BEGSR
056100151027      *
056200151027     C                   MOVEL     TNSC40        KPJBU
056300151027      *
056400151027     C* RICHIAMO PGM X CHIUSURA IN LR
056500151027     C                   CLEAR                   TIBS69
056600151027     C                   MOVEL     'C'           I69TLA
056700151027     C                   CALL      'TIBS69R'
056800151027     C                   PARM                    TIBS69
056900151027     C                   PARM                    CNACO
057000151027     C                   PARM                    CNIND
057100151027     C                   PARM                    CNCLP
057200151027     C                   PARM                    FNCLS
057300151027      *
057400151027     c                   ENDSR
