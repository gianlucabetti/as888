000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200161205     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP(*CALLER)
000300001221     H*--------------------------------------------------------------*
000400001221     H*         - RITORNO DATI AL CLIENTE (ACCETTAZIONE BOLLE)
000500000000     H*--------------------------------------------------------------*
000600001218     FTABEL00F  IF   E           K DISK
000700120323     FFIAR901L  IF   E           K DISK    USROPN
000800120323     F                                     extfile(LibFileAR9)
000900120323     FFIAR401L  IF   E           K DISK    USROPN
001000120323     F                                     extfile(LibFileAR4)
001100161205     FFNVAB00T  UF A E             DISK    USROPN COMMIT
001200001218     D*--------------------
001300001218     D* SCHIERA PER REPERIMENTO CLIENTI "FIGLI" DI UN UNIFICANTE
001400001218     D*--------------------
001500090407     D KUNI            S              7  0 DIM(1000) DESCEND
001600100312     D*----------------------------------------------------------
001700120326     D TBL             S              2    DIM(50)                              TIPI BOLLA recupero
001800120326     D CBO             S              2    DIM(50)                              TIPI BOLLA recupero
001900001218     D*--------------------
002000001218     D* DS ESTERNE
002100001218     D*--------------------
002200161205     D psds           sds
002300161205     D  procname         *PROC
002400120323     D FNBLP00F      E DS
002500900517     D KPJBA         E DS
002600001221     D DS3Q          E DS
002700090217     D DSTB          E DS
002800120326     D DS3A          E DS
002900000526     D DSBL4J        E DS
003000050825     D FNVABDS       E DS                  EXTNAME(FNVAB00T)
003100161206     D iDSVGD        E DS                  INZ EXTNAME(TIVGD00F)
003200001218     D*--------------------
003300001218     D* DS DI RIDEFINIZIOINE PARAMETRI IN INPUT
003400001218     D*--------------------
003500050825     D DSINPUT         DS
003600050825     D  DATADA                        8  0
003700050825     D  DATAA                         8  0
003800050825     D  CODCLI                        7  0
003900120323     D  FLGDBR                        1
004000120323     D  FLGNRS                        1
004100050825     D  FLGUNI                        1
004200050825     D  FLGOPZ                        1
004300080115     D  CODCLIVAS                     7  0
004400100312     D  TIPFILE                       2
004500080213     D  FLGEXE                        1
004600010131     D*--------------------
004700010131     D* DS DI SCOMPOSIZIONE DATE
004800010131     D*--------------------
004900010131     D                 DS
005000010131     D  AAAA                   1      4  0
005100010131     D  MMGG                   5      8  0
005200010131     D  DATAWRK                1      8  0
005300030704     D*--------------------
005400030704     D* DS DI SCOMPOSIZIONE CAMPO VABANT
005500030704     D*--------------------
005600030704     D                 DS
005700030704     D vabTBA                  1      2  0
005800030704     D vabNBA                  3      4  0
005900030704     D vabTB2                  5      6  0
006000030704     D vabNB2                  7      8  0
006100050825     D ds_vabANT               1      9  0
006200010605     D*--------------------
006300010605     D* VARIABILI DI WRK
006400010605     D*--------------------
006500090217     D K               S              4  0
006600120323
006700120326
006800120323     D LibFileAR4      s             21A   inz
006900120326     D LibFileAR9      s             21A   inz
007000120323     D currSysNeta     s              8A   inz('*NULL')
007100120326     D LibFiles        s             10A   inz
007200120323     D*------------------
007300120323     D* LINKING A DEFINIZIONI ESTERNE
007400120323     D*------------------
007500120323     D/COPY GAITRASRC/SRCPROTOPR,UBRTVNETA
007600120323     D/COPY GAITRASRC/SRCPROTOPI,UBRTVNETA
007700120326
007800120326
007900120326     D*------------------
008000120326     D* LINKING A DEFINIZIONI ESTERNE
008100120326     D*------------------
008200120326     D QCMD            S           2048    inz  varying
008300120326     D/COPY QSYSINC/QRPGLESRC,QUSEC
008400120326     D/COPY QSYSINC/QRPGLESRC,QCAPCMD
008500120326     D PCmd01          S            256                                         Source cmd string
008600120326     D PCmd02          S             10I 0                                      Length source string
008700120326     D                                     INZ(%SIZE(PCmd01))
008800120326     D PCmd04          S             10I 0                                      Length options ctl
008900120326     D                                     INZ(%SIZE(QCAP0100))
009000120326     D PCmd05          S              8                                         Opt. control format
009100120326     D                                     INZ('CPOP0100')
009200120326     D PCmd06          S                                                        Changed cmd string
009300120326     D                                     LIKE(PCmd01)
009400120326     D PCmd07          S             10I 0                                      Length changed strin
009500120326     D                                     INZ(%SIZE(PCmd06))
009600120326     D PCmd08          S             10I 0                                      Length changed retur
009700001218
009800010605
009900010605
010000920812     C*---------------------------------------------------------------*
010100001218     C* MAIN
010200001218     C*---------------------------------------------------------------*
010300100719     C*
010400100719     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
010500100719     C
010600100719     C/EXEC SQL
010700100719     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
010800100719     C/END-EXEC
010900100719     C
011000120323     C*
011100120323     C* Reperisco il sistema AS/400 corrente
011200120323     C                   callp     UBRTVNETA_Rtv(currSysNeta)
011300120323     C                   if        %subst(currSysNeta:1:6) = 'SETRAS'
011400120323     C                   eval      LibFileAR4 = 'FILTRA201/FIAR401L'
011500120323     C                   eval      LibFileAR9 = 'FILTRA201/FIAR901L'
011600120326     C                   eval      LibFiles   = 'FILTRA201'
011700120323     C                   else
011800120323     C                   eval      LibFileAR4 = 'FILTRAPRD/FIAR401L'
011900120323     C                   eval      LibFileAR9 = 'FILTRAPRD/FIAR901L'
012000120326     C                   eval      LibFiles   = 'FILTRAPRD'
012100120323     C                   endif
012200120323     C*
012300120326     C* Aggiungo in lista la libreria dati
012400120326     C                   eval      PCmd01 = 'ADDLIBLE LIB('+%trim(LibFiles)+
012500120326     C                                      ') POSITION(*LAST) '
012600120326     C                   exsr      EXECMD
012700120326     C*
012800120323     C                   if        not %open(FIAR401L)
012900120323     C                   open      FIAR401L
013000120323     C                   endif
013100120323     C*
013200120323     C                   if        not %open(FIAR901L)
013300120323     C                   open      FIAR901L
013400120323     C                   endif
013500100719     C*
013600001218     C                   exsr      cartbl
013700001218     C                   exsr      chkpar
013800161205     C                   exsr      procedi
013900120326     C*
014000120326     C* Rimuovo dalla lista la libreria dati
014100120326     C                   eval      PCmd01 = 'RMVLIBLE LIB('+%trim(LibFiles)+')'
014200120326     C                   exsr      EXECMD
014300001218     C*
014400001218     C                   seton                                        LR
014500120326
014600120326
014700120326     C*------------------------------------------------------------------------*
014800120326     C* EXECMD - Routine di esecuzione comando di sistema
014900120326     C*------------------------------------------------------------------------*
015000120326     C     EXECMD        BEGSR
015100120326     C*
015200120326     C                   CLEAR                   QCAP0100
015300120326     C                   EVAL      QCABCSDH = *off
015400120326     C                   EVAL      QCAPA    = *off
015500120326     C                   EVAL      QCACMDSS = *off
015600120326     C                   EVAL      QCAERVED = *allX'00'
015700120326     C                   CLEAR                   QUSEC
015800120326     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
015900120326     C*
016000120326     C                   CALL      'QCAPCMD'
016100120326     C                   PARM                    PCmd01
016200120326     C                   PARM                    PCmd02
016300120326     C                   PARM                    Qcap0100
016400120326     C                   PARM                    PCmd04
016500120326     C                   PARM                    PCmd05
016600120326     C                   PARM      *BLANK        PCmd06
016700120326     C                   PARM                    PCmd07
016800120326     C                   PARM                    PCmd08
016900120326     C                   PARM                    QUSEC
017000120326     C                   IF        QUSEI <> *BLANK
017100120326     C                   ENDIF
017200120326     C*
017300120326     C                   ENDSR
017400120326     C*------------------------------------------------------------------------*
017500001218
017600001218
017700001218
017800001218     C*------------------------------------------------------------------------*
017900001218     C* CHKPAR - Routine di controllo parametri ricevuti in input
018000001218     C*------------------------------------------------------------------------*
018100001218     C     CHKPAR        BEGSR
018200001218     C*
018300001218     C* Verifico le date
018400001218     C                   if        DATAA  = *zeros
018500001218     C                   movel     *all'9'       DATAA
018600001218     C                   endif
018700050825     C*
018800050825     C* Verifico la modalità d scrittura output richiesta:
018900050825     C*   1 = crea membro "RIC" in file FNVAB00T
019000050825     C*   2 = scrivi record in file TIVGD00F
019100050825     C                   setoff                                       2526
019200050825     C                   if        FLGOPZ = '1'
019300050825     C                   seton                                        25
019400050825     C                   endif
019500050825     C                   if        FLGOPZ = '2'
019600050825     C                   seton                                        26
019700050825     C                   endif
019800001218     C*
019900001218     C                   ENDSR
020000001218     C*------------------------------------------------------------------------*
020100001218
020200001218
020300001218
020400001218     C*------------------------------------------------------------------------*
020500161205     C* PROCEDI - Routine principale
020600001218     C*------------------------------------------------------------------------*
020700001218     C     PROCEDI       BEGSR
020800161205     C*
020900161205     C* Inizializzo variabili d wrk
021000161205     C                   movel     'N'           wProcedi          1
021100161205     C*
021200161205     C* Se richiesta scrittura sul file 00T => elaboro
021300161205     C                   if        *in25 = *on
021400161205     C                   open      fnvab00t
021500161205     C                   movel     'S'           wProcedi
021600161205     C                   endif
021700161205     C*
021800161205     C* Inizializzo la transazione
021900161205     C                   if        *in26
022000161205     C                   CALL      'TIS7VASR1'
022100161205     C                   PARM      '1'           iOPZ              1
022200161205     C                   PARM                    iDSVGD
022300161205     C                   PARM      *blanks       oPRG             10
022400161205     C                   PARM                    oOK               1
022500161205     C*
022600161205     C* Verifico esito chiamata
022700161205     C                   if        %error OR oOK = *off
022800161205     C                   exsr      EXEERR
022900161205     C                   endif
023000161205     C*
023100161205     C* Se OK => proseguo
023200161206     C                   if        oOK = *on
023300161205     C                   movel     'S'           wProcedi
023400161206     C* Forzo momentaneamente il progressivo  a *blanks
023500161206     C                   eval      oPRG = *blanks
023600161205     C                   endif
023700161205     C                   endif
023800161205     C*
023900161205     C* Se ok a procedere => elaboro
024000161205     C                   if        wProcedi = 'S'
024100001218     C*
024200001218     C* Ciclo sulla schiera dei clienti figli dell'unificante preso in input
024300001218     C                   z-add     1             I
024400090407     C                   sorta     KUNI
024500001218     C                   dow       KUNI(I) > *zeros
024600100719     C*
024700120323     C                   exsr      EXEKSC
024800001218     C*
024900001218     C                   add       1             I
025000001218     C                   enddo
025100161205     C*
025200161205     C                   endif
025300161205     C*
025400161205     C* Chiudo il file di output
025500161205     C   25              close     fnvab00t
025600161205     C*
025700161205     C* Finalizzo la transazione
025800161205     C   26              CALL      'TIS7VASR1'
025900161205     C                   PARM      '3'           iOPZ
026000161205     C                   PARM                    iDSVGD
026100161205     C                   PARM                    oPRG
026200161205     C                   PARM                    oOK
026300161205     C*
026400161205     C* Verifico esito chiamata
026500161205     C                   if        %error OR oOK = *off
026600161205     C                   exsr      EXEERR
026700161205     C                   endif
026800001218     C*
026900001218     C                   ENDSR
027000001218     C*------------------------------------------------------------------------*
027100100719
027200100719
027300100719
027400100719     C*------------------------------------------------------------------------*
027500100719     C* EXEKSC   - Elaborazione x codice cliente fatturazione
027600100719     C*------------------------------------------------------------------------*
027700100719     C     EXEKSC        BEGSR
027800100719     C*
027900100719     C                   movel     'K'           wChkCli           1
028000100719     C*
028100100719     C                   exsr      SQ_KSCDSP
028200100719     C*
028300100719     C                   ENDSR
028400100719     C*------------------------------------------------------------------------*
028500100719
028600100719
028700100719
028800100719     C*------------------------------------------------------------------------*
028900100719     C* SQ_KSCDSP - Elaborazione x codice cliente fatturazione + data spedizione
029000100719     C*------------------------------------------------------------------------*
029100100719     C     SQ_KSCDSP     BEGSR
029200100719     C*
029300100719     C                   z-add     KUNI(I)       wParKSC           7 0
029400100719     C*
029500100719     C/EXEC SQL
029600100719     C+ declare C_KSCDSP cursor for
029700120323     C+ select * from filtra201/fnblp00f
029800120323     C+ where blpksc = :wParKSC and
029900130418     C+ blpdbr=0 and blpdim < blpaas*10000+blpmgs and
030000130418     C+ blpdim between :DATADA and :DATAA
030100100719     C+ for read only
030200100719     C/END-EXEC
030300100719     C
030400100719     C/EXEC SQL
030500100719     C+ open C_KSCDSP
030600100719     C/END-EXEC
030700100719     C
030800100719     C/EXEC SQL
030900120323     C+ Fetch C_KSCDSP into :FNBLP00F
031000100719     C/END-EXEC
031100100719     C*
031200100719     C                   dow       sqlcod = *zeros
031300100719     C                   exsr      verifica
031400100719     C                   if        CHKREC = 'S'
031500100719     C                   exsr      scriviVAB
031600100719     C                   endif
031700100719     C
031800100719     C/EXEC SQL
031900120323     C+ Fetch C_KSCDSP into :FNBLP00F
032000100719     C/END-EXEC
032100100719     C*
032200100719     C                   enddo
032300100719     C*
032400100719     C/EXEC SQL
032500100719     C+ close C_KSCDSP
032600100719     C/END-EXEC
032700100719     C*
032800100719     C*
032900100719     C                   ENDSR
033000100719     C*------------------------------------------------------------------------*
033100010131
033200010131
033300001218
033400001218     C*------------------------------------------------------------------------*
033500001218     C* VERIFICA - Routine di verifica validità record corrente
033600001218     C*------------------------------------------------------------------------*
033700001218     C     VERIFICA      BEGSR
033800001218     C*
033900001218     C                   movel     'S'           CHKREC            1
034000001218     C*
034100001218     C* Verifico le date
034200001218     C                   if        CHKREC = 'S'
034300130417     C***                movel     blpaas        wrkdata           8 0
034400130417     C***                move      blpmgs        wrkdata
034500130417     C                   movel     blpdim        wrkdata           8 0
034600001218     C                   if        wrkdata < DATADA or
034700001218     C                             wrkdata > DATAA
034800001218     C                   movel     'N'           CHKREC
034900001218     C                   endif
035000001218     C                   endif
035100001218     C*
035200120323     C* Verifico il codice cliente (RIDONDANTE)
035300001218     C                   if        CHKREC = 'S'
035400120323     C                   if        blpksc <> KUNI(I)
035500010301     C                   movel     'N'           CHKREC
035600010131     C                   endif
035700001218     C                   endif
035800100719     C*
035900100719     C* Verifico il codice cliente fatturazione (in caso d 'K')
036000100719     C                   if        CHKREC = 'S'
036100100719     C                   if        wChkCli  = 'K'      AND
036200120323     C                             blpksc  <> KUNI(I)
036300100719     C                   movel     'N'           CHKREC
036400100719     C                   endif
036500100719     C                   endif
036600120323     C*
036700120326     C* Verifico il tipo bolla che non deve essere di reupero
036800090217     C                   if        CHKREC = 'S'
036900120326     C                   z-add     1             K
037000161205     C                   eval      K = %lookup(blpcbo:cbo)
037100161205     C                   if        K = *zeros
037200090217     C                   movel     'N'           CHKREC
037300120326     C                   else
037400120326     C                   movel     *blanks       blpTBL            2
037500120326     C                   eval      blpTBL = tbl(K)
037600090217     C                   endif
037700090217     C                   endif
037800120326     C*
037900120326     C* Verifico se richiesto filtro sulla "borderizzazione"
038000120326     C                   if        CHKREC = 'S'
038100120326     C                   select
038200120326     C                   when      FLGDBR = 'S'      AND
038300120326     C                             blpDBR = *zeros
038400120326     C                   movel     'N'           CHKREC
038500120326     C                   when      FLGDBR = 'N'      AND
038600120326     C                             blpDBR > *zeros
038700120326     C                   movel     'N'           CHKREC
038800120326     C                   when      FLGDBR = 'T'
038900120326     C                   endsl
039000120326     C                   endif
039100120326     C*
039200120326     C* Verifico se richiesto filtro sulla serie
039300120326     C                   if        CHKREC = 'S'
039400120326     C                   select
039500120326     C                   when      FLGNRS = 'S'      AND
039600120326     C                             blpNRS = *zeros
039700120326     C                   movel     'N'           CHKREC
039800120326     C                   when      FLGNRS = 'N'      AND
039900120326     C                             blpNRS > *zeros
040000120326     C                   movel     'N'           CHKREC
040100120326     C                   when      FLGNRS = 'T'
040200120326     C                   endsl
040300120326     C                   endif
040400001218     C*
040500001218     C                   ENDSR
040600001218     C*------------------------------------------------------------------------*
040700001218
040800001218
040900001218
041000001218     C*------------------------------------------------------------------------*
041100001221     C* SCRIVIVAB - Routine di scrittura file esiti di consegna (FNVAB)
041200001218     C*------------------------------------------------------------------------*
041300001221     C     SCRIVIVAB     BEGSR
041400001218     C*
041500001227     C                   clear                   fnvab000
041600010131     C                   setoff                                       55
041700001218     C*
041800120323     C                   movel     blpcbo        vabcbo
041900120323     C                   movel     blprsd        vabrsd
042000120323     C                   movel     blpind        vabind
042100120323     C                   movel     blpcad        vabcad
042200120323     C                   movel     blplod        vablod
042300120323     C                   movel     blpprd        vabprd
042400120323     C                   movel     blpnzd        vabnzd
042500120323     C                   movel     blpgc1        vabgc1
042600120323     C                   movel     blpgc2        vabgc2
042700120323     C                   movel     blptsp        vabtsp
042800120323     C                   movel     blpnas        vabnas
042900120323     C                   movel     blpctm        vabctm
043000120323     C                   movel     blpffd        vabffd
043100120323     C                   movel     blptcr        vabtcr
043200120323     C                   movel     blpcts        vabcts
043300120323     C                   movel     blpftm        vabftm
043400120323     C                   movel     blpgma        vabgma
043500120323     C                   movel     blpgga        vabgga
043600120323     C                   movel     blpgva        vabgva
043700010111     C                   move      ' '           vabgva
043800120326     C                   movel     blptc1        vabtc1
043900120323     C                   movel     blptc2        vabtc2
044000120323     C                   z-add     blpksc        vabccm
044100120323     C                   z-add     blplnp        vablnp
044200120404     C                   movel     blpdim        vabaas
044300120404     C                   move      blpdim        vabmgs
044400120323     C                   z-add     blpnrs        vabnrs
044500120323     C                   z-add     blpnsp        vabnsp
044600120323     C                   z-add     blplna        vablna
044700120323     C                   z-add     blpctr        vabctr
044800120323     C                   z-add     blpncl        vabncl
044900120323     C                   z-add     blppkf        vabpkb
045000120323     C                   z-add     blpvlf        vabvlb
045100120323     C                   z-add     blpqft        vabqft
045200120323     C                   z-add     blprmn        vabrmn
045300120323     C                   z-add     blpncd        vabncd
045400120323     C                   z-add     blpnca        vabnca
045500120323     C                   z-add     blpznc        vabznc
045600120323     C                   z-add     blpdbr        vabdcr
045700120326     C                   z-add     blpIAS        vabIAS
045800120323     C                   movel     blpVAS        vabVAS
045900120323     C                   movel     blpXCO        vabXCO
046000120323     C                   movel     blpRMA        vabRMA
046100130417     C                   movel     blpRMO        vabRMO
046200120326     C                   movel     blpSCL        vabSCL
046300001227     C* Considerazioni per bolle "autogenerate"
046400120323     C                   if        blpSOP = '&' and
046500120326     C                             vabSCL = *blanks
046600001227     C                   movel     'A'           vabSCL
046700001227     C                   endif
046800001227     C*
046900001227     C                   exsr      REPCSB
047000081006     C                   exsr      REPDEST
047100001218     C*
047200010131     C                   if        *in55 = *off
047300050825     C   25              WRITE     FNVAB000
047400050825     C   26              exsr      WRITIVGD
047500010131     C                   endif
047600001218     C*
047700001218     C                   ENDSR
047800001221     C*------------------------------------------------------------------------*
047900050825
048000050825
048100050825
048200050825     C*------------------------------------------------------------------------*
048300050825     C* WRITIVGD - Routine di scrittura file TIVGD (file VAS generico download)
048400050825     C*------------------------------------------------------------------------*
048500050825     C     WRITIVGD      BEGSR
048600161205     C*
048700161205     C                   clear                   iDSVGD
048800161205     C                   eval      vgdDTA = %subst(FNVABDS:1:%size(FNVABDS))
048900161205     C                   eval      vgdTIP = TIPFILE
049000161205     C                   movel     *all'0'       vgdKSU
049100161205     C                   move      CODCLIVAS     vgdKSU
049200161205     C                   eval      vgdTSC = 'WW'
049300161205     C                   eval      vgdDAT = datcor
049400161205     C                   eval      vgdPRG = oPRG
049500161205     C                   eval      vgdPGM = procname
049600161205     C*
049700161205     C                   CALL      'TIS7VASR1'
049800161205     C                   PARM      '2'           iOPZ
049900161205     C                   PARM                    iDSVGD
050000161205     C                   PARM                    oPRG
050100161205     C                   PARM                    oOK
050200161205     C*
050300161205     C* Verifico esito chiamata
050400161205     C                   if        %error OR oOK = *off
050500161205     C                   exsr      EXEERR
050600161205     C                   endif
050700050825     C*
050800050825     C                   ENDSR
050900050825     C*------------------------------------------------------------------------*
051000001218
051100001218
051200001218
051300001218     C*------------------------------------------------------------------------*
051400120323     C* REPDEST - Routine di reperimento dati relativi al destinatario della bolla (FIAR4)
051500120326     C*           ... e ai dati del mittente originale
051600001218     C*------------------------------------------------------------------------*
051700001218     C     REPDEST       BEGSR
051800120323     C*
051900120323     C                   eval      ar4TRC = 'D'
052000120323     C     KeyAR401_C    chain     fiar401l
052100120323     C                   if        %found(fiar401l)
052200120323     C                   eval      vabRD2 = ar4NOT
052300001218     C                   endif
052400001218     C*
052500001218     C                   ENDSR
052600001218     C*------------------------------------------------------------------------*
052700001218
052800001218
052900001218
053000001218     C*------------------------------------------------------------------------*
053100120323     C* REPCSB - Routine di reperimento dati relativi al contrassegno
053200001218     C*------------------------------------------------------------------------*
053300001218     C     REPCSB        BEGSR
053400001218     C*
053500120323     C     KCSB          chain     fiar901l
053600120323     C                   if        %found(fiar901l)
053700120323     C                   movel     ar9TIC        vabTIC
053800120323     C                   z-add     ar9CAS        vabCAS
053900120323     C                   movel     ar9VCA        vabVCA
054000120323     C                   movel     ar9GCA        vabGCA
054100001218     C                   endif
054200001218     C*
054300001218     C                   ENDSR
054400001218     C*------------------------------------------------------------------------*
054500010605
054600001218
054700001218
054800001218     C*------------------------------------------------------------------------*
054900001218     C* CARTBL - Routine di caricamento dati tabellati
055000001218     C*------------------------------------------------------------------------*
055100001218     C     CARTBL        BEGSR
055200001218     C*
055300001221     C                   Z-ADD     0             I                 4 0
055400050825     C                   IF        FLGUNI = 'S'
055500010301     C                   ADD       1             I
055600050825     C                   MOVEL     CODCLI        KUNI(I)
055700010301     C                   ELSE
055800001221     C                   MOVEL     '3Q'          COD
055900001218     C     KTAB          CHAIN     TABEL                              31
056000001218     C     *IN31         DOWEQ     '0'
056100090518     C                   IF        TBLFLG = ' '
056200090518     C***                IF        TBLFLG = ' ' AND
056300090518     C***                          %subst(TBLKEY:8:1) = ' '
056400001222     C                   MOVEL     TBLUNI        DS3Q
056500050825     C     §3QCKS        IFEQ      CODCLI
056600001221     C                   ADD       1             I
056700001221     C                   MOVEL     TBLKEY        KUNI(I)
056800001221     C                   ENDIF
056900001218     C                   ENDIF
057000001218     C     KTAB          READE     TABEL                                  31
057100001218     C                   ENDDO
057200010301     C                   ENDIF
057300120326     C*
057400120326     C* Carico schiera tipi bolla nn validi ai fini della spedizione
057500120326     C                   CLEAR                   K
057600120326     C                   MOVEL     '3A'          cod
057700120326     C     ktab          SETLL     tabel00f
057800120326     C     ktab          READE     tabel00f
057900120326     C                   DOW       not %eof(tabel00f)
058000120326     C                   IF        tblflg<>'*'
058100120326     C                   MOVEL     tbluni        ds3A
058200120326     C                   IF        §3arbl='R' or
058300120326     C                             §3arbl='C'
058400120326     C                   ELSE
058500090217     C                   ADD       1             K
058600120326     C                   MOVEL     TBLKEY        CBO(K)
058700120326     C                   MOVEL     §3ATB1        TBL(K)
058800120326     C                   ENDIF
058900120326     C                   ENDIF
059000120326     C     ktab          READE     tabel00f
059100120326     C                   ENDDO
059200120326     C
059300001218     C*
059400001218     C                   ENDSR
059500001221     C*------------------------------------------------------------------------*
059600161205
059700161205
059800161205
059900161205     C*------------------------------------------------------------------------*
060000161205     C* EXEERR - Routine di esecuzione azioni su Errore
060100161205     C*------------------------------------------------------------------------*
060200161205     C     EXEERR        BEGSR
060300161205     C*
060400161205     C                   dump(A)
060500161205     C                   rolbk
060600161205     C                   seton                                        lr
060700161205     C                   return
060800161205     C*
060900161205     C                   ENDSR
061000161205     C*------------------------------------------------------------------------*
061100001218
061200001218
061300001218
061400001218     C*------------------------------------------------------------------------*
061500001218     C* *INZSR - ROUTINE INIZIALE
061600001218     C*------------------------------------------------------------------------*
061700001218     C     *INZSR        BEGSR
061800001218     C*
061900001218     C     *ENTRY        PLIST
062000100312     C                   PARM                    PARAM            37
062100001218     C*
062200001218     C                   MOVEL     PARAM         DSINPUT
062300001218     C                   Z-ADD     1             CODUT
062400001218     C*
062500001218     C     KCSB          KLIST
062600120323     C                   KFLD                    blpAAS
062700120323     C                   KFLD                    blpLNP
062800120323     C                   KFLD                    blpNRS
062900120323     C                   KFLD                    blpNSP
063000001218     C*
063100001218     C     KTAB          KLIST
063200001218     C                   KFLD                    CODUT             1 0
063300001218     C                   KFLD                    COD               2
063400120323     C*
063500120323     C* Chiave su FIAR401L - Completa
063600120323     C     KeyAR401_C    klist
063700120323     C                   kfld                    blpAAS
063800120323     C                   kfld                    blpLNP
063900120323     C                   kfld                    blpNRS
064000120323     C                   kfld                    blpNSP
064100120323     C                   kfld                    ar4TRC
064200001218     C*
064300001218     C* Determino la data corrente
064400100312     C                   Z-ADD     *zeros        DATCOR            8 0
064500100312     C                   EVAL      DATCOR = %dec(%date() : *ISO)
064600001218     C*
064700001218     C                   ENDSR
