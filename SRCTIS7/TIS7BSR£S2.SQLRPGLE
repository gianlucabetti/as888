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
023200161205     C                   if        oOK = *on AND oPRG <> *blanks
023300161205     C                   movel     'S'           wProcedi
023400161205     C                   endif
023500161205     C                   endif
023600161205     C*
023700161205     C* Se ok a procedere => elaboro
023800161205     C                   if        wProcedi = 'S'
023900001218     C*
024000001218     C* Ciclo sulla schiera dei clienti figli dell'unificante preso in input
024100001218     C                   z-add     1             I
024200090407     C                   sorta     KUNI
024300001218     C                   dow       KUNI(I) > *zeros
024400100719     C*
024500120323     C                   exsr      EXEKSC
024600001218     C*
024700001218     C                   add       1             I
024800001218     C                   enddo
024900161205     C*
025000161205     C                   endif
025100161205     C*
025200161205     C* Chiudo il file di output
025300161205     C   25              close     fnvab00t
025400161205     C*
025500161205     C* Finalizzo la transazione
025600161205     C   26              CALL      'TIS7VASR1'
025700161205     C                   PARM      '3'           iOPZ
025800161205     C                   PARM                    iDSVGD
025900161205     C                   PARM                    oPRG
026000161205     C                   PARM                    oOK
026100161205     C*
026200161205     C* Verifico esito chiamata
026300161205     C                   if        %error OR oOK = *off
026400161205     C                   exsr      EXEERR
026500161205     C                   endif
026600001218     C*
026700001218     C                   ENDSR
026800001218     C*------------------------------------------------------------------------*
026900100719
027000100719
027100100719
027200100719     C*------------------------------------------------------------------------*
027300100719     C* EXEKSC   - Elaborazione x codice cliente fatturazione
027400100719     C*------------------------------------------------------------------------*
027500100719     C     EXEKSC        BEGSR
027600100719     C*
027700100719     C                   movel     'K'           wChkCli           1
027800100719     C*
027900100719     C                   exsr      SQ_KSCDSP
028000100719     C*
028100100719     C                   ENDSR
028200100719     C*------------------------------------------------------------------------*
028300100719
028400100719
028500100719
028600100719     C*------------------------------------------------------------------------*
028700100719     C* SQ_KSCDSP - Elaborazione x codice cliente fatturazione + data spedizione
028800100719     C*------------------------------------------------------------------------*
028900100719     C     SQ_KSCDSP     BEGSR
029000100719     C*
029100100719     C                   z-add     KUNI(I)       wParKSC           7 0
029200100719     C*
029300100719     C/EXEC SQL
029400100719     C+ declare C_KSCDSP cursor for
029500120323     C+ select * from filtra201/fnblp00f
029600120323     C+ where blpksc = :wParKSC and
029700130418     C+ blpdbr=0 and blpdim < blpaas*10000+blpmgs and
029800130418     C+ blpdim between :DATADA and :DATAA
029900100719     C+ for read only
030000100719     C/END-EXEC
030100100719     C
030200100719     C/EXEC SQL
030300100719     C+ open C_KSCDSP
030400100719     C/END-EXEC
030500100719     C
030600100719     C/EXEC SQL
030700120323     C+ Fetch C_KSCDSP into :FNBLP00F
030800100719     C/END-EXEC
030900100719     C*
031000100719     C                   dow       sqlcod = *zeros
031100100719     C                   exsr      verifica
031200100719     C                   if        CHKREC = 'S'
031300100719     C                   exsr      scriviVAB
031400100719     C                   endif
031500100719     C
031600100719     C/EXEC SQL
031700120323     C+ Fetch C_KSCDSP into :FNBLP00F
031800100719     C/END-EXEC
031900100719     C*
032000100719     C                   enddo
032100100719     C*
032200100719     C/EXEC SQL
032300100719     C+ close C_KSCDSP
032400100719     C/END-EXEC
032500100719     C*
032600100719     C*
032700100719     C                   ENDSR
032800100719     C*------------------------------------------------------------------------*
032900010131
033000010131
033100001218
033200001218     C*------------------------------------------------------------------------*
033300001218     C* VERIFICA - Routine di verifica validità record corrente
033400001218     C*------------------------------------------------------------------------*
033500001218     C     VERIFICA      BEGSR
033600001218     C*
033700001218     C                   movel     'S'           CHKREC            1
033800001218     C*
033900001218     C* Verifico le date
034000001218     C                   if        CHKREC = 'S'
034100130417     C***                movel     blpaas        wrkdata           8 0
034200130417     C***                move      blpmgs        wrkdata
034300130417     C                   movel     blpdim        wrkdata           8 0
034400001218     C                   if        wrkdata < DATADA or
034500001218     C                             wrkdata > DATAA
034600001218     C                   movel     'N'           CHKREC
034700001218     C                   endif
034800001218     C                   endif
034900001218     C*
035000120323     C* Verifico il codice cliente (RIDONDANTE)
035100001218     C                   if        CHKREC = 'S'
035200120323     C                   if        blpksc <> KUNI(I)
035300010301     C                   movel     'N'           CHKREC
035400010131     C                   endif
035500001218     C                   endif
035600100719     C*
035700100719     C* Verifico il codice cliente fatturazione (in caso d 'K')
035800100719     C                   if        CHKREC = 'S'
035900100719     C                   if        wChkCli  = 'K'      AND
036000120323     C                             blpksc  <> KUNI(I)
036100100719     C                   movel     'N'           CHKREC
036200100719     C                   endif
036300100719     C                   endif
036400120323     C*
036500120326     C* Verifico il tipo bolla che non deve essere di reupero
036600090217     C                   if        CHKREC = 'S'
036700120326     C                   z-add     1             K
036800161205     C                   eval      K = %lookup(blpcbo:cbo)
036900161205     C                   if        K = *zeros
037000090217     C                   movel     'N'           CHKREC
037100120326     C                   else
037200120326     C                   movel     *blanks       blpTBL            2
037300120326     C                   eval      blpTBL = tbl(K)
037400090217     C                   endif
037500090217     C                   endif
037600120326     C*
037700120326     C* Verifico se richiesto filtro sulla "borderizzazione"
037800120326     C                   if        CHKREC = 'S'
037900120326     C                   select
038000120326     C                   when      FLGDBR = 'S'      AND
038100120326     C                             blpDBR = *zeros
038200120326     C                   movel     'N'           CHKREC
038300120326     C                   when      FLGDBR = 'N'      AND
038400120326     C                             blpDBR > *zeros
038500120326     C                   movel     'N'           CHKREC
038600120326     C                   when      FLGDBR = 'T'
038700120326     C                   endsl
038800120326     C                   endif
038900120326     C*
039000120326     C* Verifico se richiesto filtro sulla serie
039100120326     C                   if        CHKREC = 'S'
039200120326     C                   select
039300120326     C                   when      FLGNRS = 'S'      AND
039400120326     C                             blpNRS = *zeros
039500120326     C                   movel     'N'           CHKREC
039600120326     C                   when      FLGNRS = 'N'      AND
039700120326     C                             blpNRS > *zeros
039800120326     C                   movel     'N'           CHKREC
039900120326     C                   when      FLGNRS = 'T'
040000120326     C                   endsl
040100120326     C                   endif
040200001218     C*
040300001218     C                   ENDSR
040400001218     C*------------------------------------------------------------------------*
040500001218
040600001218
040700001218
040800001218     C*------------------------------------------------------------------------*
040900001221     C* SCRIVIVAB - Routine di scrittura file esiti di consegna (FNVAB)
041000001218     C*------------------------------------------------------------------------*
041100001221     C     SCRIVIVAB     BEGSR
041200001218     C*
041300001227     C                   clear                   fnvab000
041400010131     C                   setoff                                       55
041500001218     C*
041600120323     C                   movel     blpcbo        vabcbo
041700120323     C                   movel     blprsd        vabrsd
041800120323     C                   movel     blpind        vabind
041900120323     C                   movel     blpcad        vabcad
042000120323     C                   movel     blplod        vablod
042100120323     C                   movel     blpprd        vabprd
042200120323     C                   movel     blpnzd        vabnzd
042300120323     C                   movel     blpgc1        vabgc1
042400120323     C                   movel     blpgc2        vabgc2
042500120323     C                   movel     blptsp        vabtsp
042600120323     C                   movel     blpnas        vabnas
042700120323     C                   movel     blpctm        vabctm
042800120323     C                   movel     blpffd        vabffd
042900120323     C                   movel     blptcr        vabtcr
043000120323     C                   movel     blpcts        vabcts
043100120323     C                   movel     blpftm        vabftm
043200120323     C                   movel     blpgma        vabgma
043300120323     C                   movel     blpgga        vabgga
043400120323     C                   movel     blpgva        vabgva
043500010111     C                   move      ' '           vabgva
043600120326     C                   movel     blptc1        vabtc1
043700120323     C                   movel     blptc2        vabtc2
043800120323     C                   z-add     blpksc        vabccm
043900120323     C                   z-add     blplnp        vablnp
044000120404     C                   movel     blpdim        vabaas
044100120404     C                   move      blpdim        vabmgs
044200120323     C                   z-add     blpnrs        vabnrs
044300120323     C                   z-add     blpnsp        vabnsp
044400120323     C                   z-add     blplna        vablna
044500120323     C                   z-add     blpctr        vabctr
044600120323     C                   z-add     blpncl        vabncl
044700120323     C                   z-add     blppkf        vabpkb
044800120323     C                   z-add     blpvlf        vabvlb
044900120323     C                   z-add     blpqft        vabqft
045000120323     C                   z-add     blprmn        vabrmn
045100120323     C                   z-add     blpncd        vabncd
045200120323     C                   z-add     blpnca        vabnca
045300120323     C                   z-add     blpznc        vabznc
045400120323     C                   z-add     blpdbr        vabdcr
045500120326     C                   z-add     blpIAS        vabIAS
045600120323     C                   movel     blpVAS        vabVAS
045700120323     C                   movel     blpXCO        vabXCO
045800120323     C                   movel     blpRMA        vabRMA
045900130417     C                   movel     blpRMO        vabRMO
046000120326     C                   movel     blpSCL        vabSCL
046100001227     C* Considerazioni per bolle "autogenerate"
046200120323     C                   if        blpSOP = '&' and
046300120326     C                             vabSCL = *blanks
046400001227     C                   movel     'A'           vabSCL
046500001227     C                   endif
046600001227     C*
046700001227     C                   exsr      REPCSB
046800081006     C                   exsr      REPDEST
046900001218     C*
047000010131     C                   if        *in55 = *off
047100050825     C   25              WRITE     FNVAB000
047200050825     C   26              exsr      WRITIVGD
047300010131     C                   endif
047400001218     C*
047500001218     C                   ENDSR
047600001221     C*------------------------------------------------------------------------*
047700050825
047800050825
047900050825
048000050825     C*------------------------------------------------------------------------*
048100050825     C* WRITIVGD - Routine di scrittura file TIVGD (file VAS generico download)
048200050825     C*------------------------------------------------------------------------*
048300050825     C     WRITIVGD      BEGSR
048400161205     C*
048500161205     C                   clear                   iDSVGD
048600161205     C                   eval      vgdDTA = %subst(FNVABDS:1:%size(FNVABDS))
048700161205     C                   eval      vgdTIP = TIPFILE
048800161205     C                   movel     *all'0'       vgdKSU
048900161205     C                   move      CODCLIVAS     vgdKSU
049000161205     C                   eval      vgdTSC = 'WW'
049100161205     C                   eval      vgdDAT = datcor
049200161205     C                   eval      vgdPRG = oPRG
049300161205     C                   eval      vgdPGM = procname
049400161205     C*
049500161205     C                   CALL      'TIS7VASR1'
049600161205     C                   PARM      '2'           iOPZ
049700161205     C                   PARM                    iDSVGD
049800161205     C                   PARM                    oPRG
049900161205     C                   PARM                    oOK
050000161205     C*
050100161205     C* Verifico esito chiamata
050200161205     C                   if        %error OR oOK = *off
050300161205     C                   exsr      EXEERR
050400161205     C                   endif
050500050825     C*
050600050825     C                   ENDSR
050700050825     C*------------------------------------------------------------------------*
050800001218
050900001218
051000001218
051100001218     C*------------------------------------------------------------------------*
051200120323     C* REPDEST - Routine di reperimento dati relativi al destinatario della bolla (FIAR4)
051300120326     C*           ... e ai dati del mittente originale
051400001218     C*------------------------------------------------------------------------*
051500001218     C     REPDEST       BEGSR
051600120323     C*
051700120323     C                   eval      ar4TRC = 'D'
051800120323     C     KeyAR401_C    chain     fiar401l
051900120323     C                   if        %found(fiar401l)
052000120323     C                   eval      vabRD2 = ar4NOT
052100001218     C                   endif
052200001218     C*
052300001218     C                   ENDSR
052400001218     C*------------------------------------------------------------------------*
052500001218
052600001218
052700001218
052800001218     C*------------------------------------------------------------------------*
052900120323     C* REPCSB - Routine di reperimento dati relativi al contrassegno
053000001218     C*------------------------------------------------------------------------*
053100001218     C     REPCSB        BEGSR
053200001218     C*
053300120323     C     KCSB          chain     fiar901l
053400120323     C                   if        %found(fiar901l)
053500120323     C                   movel     ar9TIC        vabTIC
053600120323     C                   z-add     ar9CAS        vabCAS
053700120323     C                   movel     ar9VCA        vabVCA
053800120323     C                   movel     ar9GCA        vabGCA
053900001218     C                   endif
054000001218     C*
054100001218     C                   ENDSR
054200001218     C*------------------------------------------------------------------------*
054300010605
054400001218
054500001218
054600001218     C*------------------------------------------------------------------------*
054700001218     C* CARTBL - Routine di caricamento dati tabellati
054800001218     C*------------------------------------------------------------------------*
054900001218     C     CARTBL        BEGSR
055000001218     C*
055100001221     C                   Z-ADD     0             I                 4 0
055200050825     C                   IF        FLGUNI = 'S'
055300010301     C                   ADD       1             I
055400050825     C                   MOVEL     CODCLI        KUNI(I)
055500010301     C                   ELSE
055600001221     C                   MOVEL     '3Q'          COD
055700001218     C     KTAB          CHAIN     TABEL                              31
055800001218     C     *IN31         DOWEQ     '0'
055900090518     C                   IF        TBLFLG = ' '
056000090518     C***                IF        TBLFLG = ' ' AND
056100090518     C***                          %subst(TBLKEY:8:1) = ' '
056200001222     C                   MOVEL     TBLUNI        DS3Q
056300050825     C     §3QCKS        IFEQ      CODCLI
056400001221     C                   ADD       1             I
056500001221     C                   MOVEL     TBLKEY        KUNI(I)
056600001221     C                   ENDIF
056700001218     C                   ENDIF
056800001218     C     KTAB          READE     TABEL                                  31
056900001218     C                   ENDDO
057000010301     C                   ENDIF
057100120326     C*
057200120326     C* Carico schiera tipi bolla nn validi ai fini della spedizione
057300120326     C                   CLEAR                   K
057400120326     C                   MOVEL     '3A'          cod
057500120326     C     ktab          SETLL     tabel00f
057600120326     C     ktab          READE     tabel00f
057700120326     C                   DOW       not %eof(tabel00f)
057800120326     C                   IF        tblflg<>'*'
057900120326     C                   MOVEL     tbluni        ds3A
058000120326     C                   IF        §3arbl='R' or
058100120326     C                             §3arbl='C'
058200120326     C                   ELSE
058300090217     C                   ADD       1             K
058400120326     C                   MOVEL     TBLKEY        CBO(K)
058500120326     C                   MOVEL     §3ATB1        TBL(K)
058600120326     C                   ENDIF
058700120326     C                   ENDIF
058800120326     C     ktab          READE     tabel00f
058900120326     C                   ENDDO
059000120326     C
059100001218     C*
059200001218     C                   ENDSR
059300001221     C*------------------------------------------------------------------------*
059400161205
059500161205
059600161205
059700161205     C*------------------------------------------------------------------------*
059800161205     C* EXEERR - Routine di esecuzione azioni su Errore
059900161205     C*------------------------------------------------------------------------*
060000161205     C     EXEERR        BEGSR
060100161205     C*
060200161205     C                   dump(A)
060300161205     C                   rolbk
060400161205     C                   seton                                        lr
060500161205     C                   return
060600161205     C*
060700161205     C                   ENDSR
060800161205     C*------------------------------------------------------------------------*
060900001218
061000001218
061100001218
061200001218     C*------------------------------------------------------------------------*
061300001218     C* *INZSR - ROUTINE INIZIALE
061400001218     C*------------------------------------------------------------------------*
061500001218     C     *INZSR        BEGSR
061600001218     C*
061700001218     C     *ENTRY        PLIST
061800100312     C                   PARM                    PARAM            37
061900001218     C*
062000001218     C                   MOVEL     PARAM         DSINPUT
062100001218     C                   Z-ADD     1             CODUT
062200001218     C*
062300001218     C     KCSB          KLIST
062400120323     C                   KFLD                    blpAAS
062500120323     C                   KFLD                    blpLNP
062600120323     C                   KFLD                    blpNRS
062700120323     C                   KFLD                    blpNSP
062800001218     C*
062900001218     C     KTAB          KLIST
063000001218     C                   KFLD                    CODUT             1 0
063100001218     C                   KFLD                    COD               2
063200120323     C*
063300120323     C* Chiave su FIAR401L - Completa
063400120323     C     KeyAR401_C    klist
063500120323     C                   kfld                    blpAAS
063600120323     C                   kfld                    blpLNP
063700120323     C                   kfld                    blpNRS
063800120323     C                   kfld                    blpNSP
063900120323     C                   kfld                    ar4TRC
064000001218     C*
064100001218     C* Determino la data corrente
064200100312     C                   Z-ADD     *zeros        DATCOR            8 0
064300100312     C                   EVAL      DATCOR = %dec(%date() : *ISO)
064400001218     C*
064500001218     C                   ENDSR
