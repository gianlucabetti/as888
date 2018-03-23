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
001200161206     FTIVGD00F  O    E             DISK    USROPN COMMIT
001300001218     D*--------------------
001400001218     D* SCHIERA PER REPERIMENTO CLIENTI "FIGLI" DI UN UNIFICANTE
001500001218     D*--------------------
001600090407     D KUNI            S              7  0 DIM(1000) DESCEND
001700100312     D*----------------------------------------------------------
001800120326     D TBL             S              2    DIM(50)                              TIPI BOLLA recupero
001900120326     D CBO             S              2    DIM(50)                              TIPI BOLLA recupero
002000001218     D*--------------------
002100001218     D* DS ESTERNE
002200001218     D*--------------------
002300161205     D psds           sds
002400161205     D  procname         *PROC
002500120323     D FNBLP00F      E DS
002600900517     D KPJBA         E DS
002700001221     D DS3Q          E DS
002800090217     D DSTB          E DS
002900120326     D DS3A          E DS
003000000526     D DSBL4J        E DS
003100050825     D FNVABDS       E DS                  EXTNAME(FNVAB00T)
003200170227     D TIS7VASDS     E DS
003300001218     D*--------------------
003400001218     D* DS DI RIDEFINIZIOINE PARAMETRI IN INPUT
003500001218     D*--------------------
003600050825     D DSINPUT         DS
003700050825     D  DATADA                        8  0
003800050825     D  DATAA                         8  0
003900050825     D  CODCLI                        7  0
004000120323     D  FLGDBR                        1
004100120323     D  FLGNRS                        1
004200050825     D  FLGUNI                        1
004300050825     D  FLGOPZ                        1
004400080115     D  CODCLIVAS                     7  0
004500100312     D  TIPFILE                       2
004600080213     D  FLGEXE                        1
004700010131     D*--------------------
004800010131     D* DS DI SCOMPOSIZIONE DATE
004900010131     D*--------------------
005000010131     D                 DS
005100010131     D  AAAA                   1      4  0
005200010131     D  MMGG                   5      8  0
005300010131     D  DATAWRK                1      8  0
005400030704     D*--------------------
005500030704     D* DS DI SCOMPOSIZIONE CAMPO VABANT
005600030704     D*--------------------
005700030704     D                 DS
005800030704     D vabTBA                  1      2  0
005900030704     D vabNBA                  3      4  0
006000030704     D vabTB2                  5      6  0
006100030704     D vabNB2                  7      8  0
006200050825     D ds_vabANT               1      9  0
006300010605     D*--------------------
006400010605     D* VARIABILI DI WRK
006500010605     D*--------------------
006600090217     D K               S              4  0
006700120323
006800120326
006900120323     D LibFileAR4      s             21A   inz
007000120326     D LibFileAR9      s             21A   inz
007100120323     D currSysNeta     s              8A   inz('*NULL')
007200120326     D LibFiles        s             10A   inz
007300120323     D*------------------
007400120323     D* LINKING A DEFINIZIONI ESTERNE
007500120323     D*------------------
007600120323     D/COPY GAITRASRC/SRCPROTOPR,UBRTVNETA
007700120323     D/COPY GAITRASRC/SRCPROTOPI,UBRTVNETA
007800120326
007900120326
008000120326     D*------------------
008100120326     D* LINKING A DEFINIZIONI ESTERNE
008200120326     D*------------------
008300120326     D QCMD            S           2048    inz  varying
008400120326     D/COPY QSYSINC/QRPGLESRC,QUSEC
008500120326     D/COPY QSYSINC/QRPGLESRC,QCAPCMD
008600120326     D PCmd01          S            256                                         Source cmd string
008700120326     D PCmd02          S             10I 0                                      Length source string
008800120326     D                                     INZ(%SIZE(PCmd01))
008900120326     D PCmd04          S             10I 0                                      Length options ctl
009000120326     D                                     INZ(%SIZE(QCAP0100))
009100120326     D PCmd05          S              8                                         Opt. control format
009200120326     D                                     INZ('CPOP0100')
009300120326     D PCmd06          S                                                        Changed cmd string
009400120326     D                                     LIKE(PCmd01)
009500120326     D PCmd07          S             10I 0                                      Length changed strin
009600120326     D                                     INZ(%SIZE(PCmd06))
009700120326     D PCmd08          S             10I 0                                      Length changed retur
009800001218
009900010605
010000010605
010100920812     C*---------------------------------------------------------------*
010200001218     C* MAIN
010300001218     C*---------------------------------------------------------------*
010400100719     C*
010500100719     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
010600100719     C
010700100719     C/EXEC SQL
010800100719     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
010900100719     C/END-EXEC
011000100719     C
011100120323     C*
011200120323     C* Reperisco il sistema AS/400 corrente
011300120323     C                   callp     UBRTVNETA_Rtv(currSysNeta)
011400120323     C                   if        %subst(currSysNeta:1:6) = 'SETRAS'
011500120323     C                   eval      LibFileAR4 = 'FILTRA201/FIAR401L'
011600120323     C                   eval      LibFileAR9 = 'FILTRA201/FIAR901L'
011700120326     C                   eval      LibFiles   = 'FILTRA201'
011800120323     C                   else
011900120323     C                   eval      LibFileAR4 = 'FILTRAPRD/FIAR401L'
012000120323     C                   eval      LibFileAR9 = 'FILTRAPRD/FIAR901L'
012100120326     C                   eval      LibFiles   = 'FILTRAPRD'
012200120323     C                   endif
012300120323     C*
012400120326     C* Aggiungo in lista la libreria dati
012500120326     C                   eval      PCmd01 = 'ADDLIBLE LIB('+%trim(LibFiles)+
012600120326     C                                      ') POSITION(*LAST) '
012700120326     C                   exsr      EXECMD
012800120326     C*
012900120323     C                   if        not %open(FIAR401L)
013000120323     C                   open      FIAR401L
013100120323     C                   endif
013200120323     C*
013300120323     C                   if        not %open(FIAR901L)
013400120323     C                   open      FIAR901L
013500120323     C                   endif
013600161206     C*
013700161206     C* Avvio il monitoring del processo
013800161206     C                   monitor
013900100719     C*
014000001218     C                   exsr      cartbl
014100001218     C                   exsr      chkpar
014200161205     C                   exsr      procedi
014300161206     C*
014400161206     C* Sabcisco il commit
014500170309     C                   commit(e)
014600161206     C*
014700161206     C* Su errore
014800161206     C                   on-error
014900161206     C                   exsr      exeerr
015000161206     C*
015100161206     C* Fine monitoring
015200161206     C                   endmon
015300120326     C*
015400120326     C* Rimuovo dalla lista la libreria dati
015500120326     C                   eval      PCmd01 = 'RMVLIBLE LIB('+%trim(LibFiles)+')'
015600120326     C                   exsr      EXECMD
015700001218     C*
015800001218     C                   seton                                        LR
015900120326
016000120326
016100120326     C*------------------------------------------------------------------------*
016200120326     C* EXECMD - Routine di esecuzione comando di sistema
016300120326     C*------------------------------------------------------------------------*
016400120326     C     EXECMD        BEGSR
016500120326     C*
016600120326     C                   CLEAR                   QCAP0100
016700120326     C                   EVAL      QCABCSDH = *off
016800120326     C                   EVAL      QCAPA    = *off
016900120326     C                   EVAL      QCACMDSS = *off
017000120326     C                   EVAL      QCAERVED = *allX'00'
017100120326     C                   CLEAR                   QUSEC
017200120326     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
017300120326     C*
017400120326     C                   CALL      'QCAPCMD'
017500120326     C                   PARM                    PCmd01
017600120326     C                   PARM                    PCmd02
017700120326     C                   PARM                    Qcap0100
017800120326     C                   PARM                    PCmd04
017900120326     C                   PARM                    PCmd05
018000120326     C                   PARM      *BLANK        PCmd06
018100120326     C                   PARM                    PCmd07
018200120326     C                   PARM                    PCmd08
018300120326     C                   PARM                    QUSEC
018400120326     C                   IF        QUSEI <> *BLANK
018500120326     C                   ENDIF
018600120326     C*
018700120326     C                   ENDSR
018800120326     C*------------------------------------------------------------------------*
018900001218
019000001218
019100001218
019200001218     C*------------------------------------------------------------------------*
019300001218     C* CHKPAR - Routine di controllo parametri ricevuti in input
019400001218     C*------------------------------------------------------------------------*
019500001218     C     CHKPAR        BEGSR
019600001218     C*
019700001218     C* Verifico le date
019800001218     C                   if        DATAA  = *zeros
019900001218     C                   movel     *all'9'       DATAA
020000001218     C                   endif
020100050825     C*
020200050825     C* Verifico la modalità d scrittura output richiesta:
020300050825     C*   1 = crea membro "RIC" in file FNVAB00T
020400050825     C*   2 = scrivi record in file TIVGD00F
020500050825     C                   setoff                                       2526
020600050825     C                   if        FLGOPZ = '1'
020700050825     C                   seton                                        25
020800050825     C                   endif
020900050825     C                   if        FLGOPZ = '2'
021000050825     C                   seton                                        26
021100050825     C                   endif
021200001218     C*
021300001218     C                   ENDSR
021400001218     C*------------------------------------------------------------------------*
021500001218
021600001218
021700001218
021800001218     C*------------------------------------------------------------------------*
021900161205     C* PROCEDI - Routine principale
022000001218     C*------------------------------------------------------------------------*
022100001218     C     PROCEDI       BEGSR
022200161205     C*
022300161205     C* Inizializzo variabili d wrk
022400161205     C                   movel     'N'           wProcedi          1
022500161205     C*
022600161205     C* Se richiesta scrittura sul file 00T => elaboro
022700161205     C                   if        *in25 = *on
022800161205     C                   open      fnvab00t
022900161205     C                   movel     'S'           wProcedi
023000161205     C                   endif
023100161205     C*
023200161205     C* Inizializzo la transazione
023300161205     C                   if        *in26
023400161206     C                   open      tivgd00f
023500170227     C*
023600170227     C* Stacco progressivo univoco download
023700170227     C                   CLEAR                   TIS7VASDS
023800170227     C                   EVAL      i§7VASOPZ = 'PRG'
023900170227     C                   CALL(e)   'TIS7VASR1'
024000170227     C                   PARM                    TIS7VASDS
024100170227     C*
024200170227     C* Se OK => proseguo
024300170227     C                   if        not %error AND
024400170227     C                             o§7VASOK = *on AND o§7VASPRG <> *blanks
024500170227     C                   movel     'S'           wProcedi
024600170227     C                   endif
024700161205     C                   endif
024800161205     C*
024900161205     C* Se ok a procedere => elaboro
025000161205     C                   if        wProcedi = 'S'
025100001218     C*
025200001218     C* Ciclo sulla schiera dei clienti figli dell'unificante preso in input
025300001218     C                   z-add     1             I
025400090407     C                   sorta     KUNI
025500001218     C                   dow       KUNI(I) > *zeros
025600100719     C*
025700120323     C                   exsr      EXEKSC
025800001218     C*
025900001218     C                   add       1             I
026000001218     C                   enddo
026100161205     C*
026200161205     C                   endif
026300161205     C*
026400161205     C* Chiudo il file di output
026500161205     C   25              close     fnvab00t
026600161206     C   26              close     tivgd00f
026700170227     C*
026800170227     C* Finalizzo la transazione
026900170227     C                   if        *in26
027000170227     C                   EVAL      i§7VASOPZ  = 'RLS'
027100170227     C                   EVAL      i§7VASTIP  = TIPFILE
027200170227     C                   EVAL      i§7VASKSU  = '0'+%editc(CODCLIVAS:'X')
027300170227     C                   EVAL      i§7VASTSC  = 'WW'
027400170227     C                   EVAL      i§7VASSTO  = '?'
027500170227     C                   EVAL      i§7VASSTTO = 'L'
027600170227     C                   EVAL      i§7VASPRG  = o§7VASPRG
027700170227     C                   CALL(e)   'TIS7VASR1'
027800170227     C                   PARM                    TIS7VASDS
027900170227     C*
028000170227     C* Verifico esito chiamata
028100170227     C                   if        %error OR o§7VASOK = *off
028200170227     C                   exsr      EXEERR
028300170227     C                   endif
028400170227     C                   endif
028500161205     C*
028600001218     C                   ENDSR
028700001218     C*------------------------------------------------------------------------*
028800100719
028900100719
029000100719
029100100719     C*------------------------------------------------------------------------*
029200100719     C* EXEKSC   - Elaborazione x codice cliente fatturazione
029300100719     C*------------------------------------------------------------------------*
029400100719     C     EXEKSC        BEGSR
029500100719     C*
029600100719     C                   movel     'K'           wChkCli           1
029700100719     C*
029800100719     C                   exsr      SQ_KSCDSP
029900100719     C*
030000100719     C                   ENDSR
030100100719     C*------------------------------------------------------------------------*
030200100719
030300100719
030400100719
030500100719     C*------------------------------------------------------------------------*
030600100719     C* SQ_KSCDSP - Elaborazione x codice cliente fatturazione + data spedizione
030700100719     C*------------------------------------------------------------------------*
030800100719     C     SQ_KSCDSP     BEGSR
030900100719     C*
031000100719     C                   z-add     KUNI(I)       wParKSC           7 0
031100100719     C*
031200100719     C/EXEC SQL
031300100719     C+ declare C_KSCDSP cursor for
031400120323     C+ select * from filtra201/fnblp00f
031500120323     C+ where blpksc = :wParKSC and
031600130418     C+ blpdbr=0 and blpdim < blpaas*10000+blpmgs and
031700130418     C+ blpdim between :DATADA and :DATAA
031800100719     C+ for read only
031900100719     C/END-EXEC
032000100719     C
032100100719     C/EXEC SQL
032200100719     C+ open C_KSCDSP
032300100719     C/END-EXEC
032400100719     C
032500100719     C/EXEC SQL
032600120323     C+ Fetch C_KSCDSP into :FNBLP00F
032700100719     C/END-EXEC
032800100719     C*
032900100719     C                   dow       sqlcod = *zeros
033000100719     C                   exsr      verifica
033100100719     C                   if        CHKREC = 'S'
033200100719     C                   exsr      scriviVAB
033300100719     C                   endif
033400100719     C
033500100719     C/EXEC SQL
033600120323     C+ Fetch C_KSCDSP into :FNBLP00F
033700100719     C/END-EXEC
033800100719     C*
033900100719     C                   enddo
034000100719     C*
034100100719     C/EXEC SQL
034200100719     C+ close C_KSCDSP
034300100719     C/END-EXEC
034400100719     C*
034500100719     C*
034600100719     C                   ENDSR
034700100719     C*------------------------------------------------------------------------*
034800010131
034900010131
035000001218
035100001218     C*------------------------------------------------------------------------*
035200001218     C* VERIFICA - Routine di verifica validità record corrente
035300001218     C*------------------------------------------------------------------------*
035400001218     C     VERIFICA      BEGSR
035500001218     C*
035600001218     C                   movel     'S'           CHKREC            1
035700001218     C*
035800001218     C* Verifico le date
035900001218     C                   if        CHKREC = 'S'
036000130417     C***                movel     blpaas        wrkdata           8 0
036100130417     C***                move      blpmgs        wrkdata
036200130417     C                   movel     blpdim        wrkdata           8 0
036300001218     C                   if        wrkdata < DATADA or
036400001218     C                             wrkdata > DATAA
036500001218     C                   movel     'N'           CHKREC
036600001218     C                   endif
036700001218     C                   endif
036800001218     C*
036900120323     C* Verifico il codice cliente (RIDONDANTE)
037000001218     C                   if        CHKREC = 'S'
037100120323     C                   if        blpksc <> KUNI(I)
037200010301     C                   movel     'N'           CHKREC
037300010131     C                   endif
037400001218     C                   endif
037500100719     C*
037600100719     C* Verifico il codice cliente fatturazione (in caso d 'K')
037700100719     C                   if        CHKREC = 'S'
037800100719     C                   if        wChkCli  = 'K'      AND
037900120323     C                             blpksc  <> KUNI(I)
038000100719     C                   movel     'N'           CHKREC
038100100719     C                   endif
038200100719     C                   endif
038300120323     C*
038400120326     C* Verifico il tipo bolla che non deve essere di reupero
038500090217     C                   if        CHKREC = 'S'
038600120326     C                   z-add     1             K
038700161205     C                   eval      K = %lookup(blpcbo:cbo)
038800161205     C                   if        K = *zeros
038900090217     C                   movel     'N'           CHKREC
039000120326     C                   else
039100120326     C                   movel     *blanks       blpTBL            2
039200120326     C                   eval      blpTBL = tbl(K)
039300090217     C                   endif
039400090217     C                   endif
039500120326     C*
039600120326     C* Verifico se richiesto filtro sulla "borderizzazione"
039700120326     C                   if        CHKREC = 'S'
039800120326     C                   select
039900120326     C                   when      FLGDBR = 'S'      AND
040000120326     C                             blpDBR = *zeros
040100120326     C                   movel     'N'           CHKREC
040200120326     C                   when      FLGDBR = 'N'      AND
040300120326     C                             blpDBR > *zeros
040400120326     C                   movel     'N'           CHKREC
040500120326     C                   when      FLGDBR = 'T'
040600120326     C                   endsl
040700120326     C                   endif
040800120326     C*
040900120326     C* Verifico se richiesto filtro sulla serie
041000120326     C                   if        CHKREC = 'S'
041100120326     C                   select
041200120326     C                   when      FLGNRS = 'S'      AND
041300120326     C                             blpNRS = *zeros
041400120326     C                   movel     'N'           CHKREC
041500120326     C                   when      FLGNRS = 'N'      AND
041600120326     C                             blpNRS > *zeros
041700120326     C                   movel     'N'           CHKREC
041800120326     C                   when      FLGNRS = 'T'
041900120326     C                   endsl
042000120326     C                   endif
042100001218     C*
042200001218     C                   ENDSR
042300001218     C*------------------------------------------------------------------------*
042400001218
042500001218
042600001218
042700001218     C*------------------------------------------------------------------------*
042800001221     C* SCRIVIVAB - Routine di scrittura file esiti di consegna (FNVAB)
042900001218     C*------------------------------------------------------------------------*
043000001221     C     SCRIVIVAB     BEGSR
043100001218     C*
043200001227     C                   clear                   fnvab000
043300010131     C                   setoff                                       55
043400001218     C*
043500120323     C                   movel     blpcbo        vabcbo
043600120323     C                   movel     blprsd        vabrsd
043700120323     C                   movel     blpind        vabind
043800120323     C                   movel     blpcad        vabcad
043900120323     C                   movel     blplod        vablod
044000120323     C                   movel     blpprd        vabprd
044100120323     C                   movel     blpnzd        vabnzd
044200120323     C                   movel     blpgc1        vabgc1
044300120323     C                   movel     blpgc2        vabgc2
044400120323     C                   movel     blptsp        vabtsp
044500120323     C                   movel     blpnas        vabnas
044600120323     C                   movel     blpctm        vabctm
044700120323     C                   movel     blpffd        vabffd
044800120323     C                   movel     blptcr        vabtcr
044900120323     C                   movel     blpcts        vabcts
045000120323     C                   movel     blpftm        vabftm
045100120323     C                   movel     blpgma        vabgma
045200120323     C                   movel     blpgga        vabgga
045300120323     C                   movel     blpgva        vabgva
045400010111     C                   move      ' '           vabgva
045500120326     C                   movel     blptc1        vabtc1
045600120323     C                   movel     blptc2        vabtc2
045700120323     C                   z-add     blpksc        vabccm
045800120323     C                   z-add     blplnp        vablnp
045900120404     C                   movel     blpdim        vabaas
046000120404     C                   move      blpdim        vabmgs
046100120323     C                   z-add     blpnrs        vabnrs
046200120323     C                   z-add     blpnsp        vabnsp
046300120323     C                   z-add     blplna        vablna
046400120323     C                   z-add     blpctr        vabctr
046500120323     C                   z-add     blpncl        vabncl
046600120323     C                   z-add     blppkf        vabpkb
046700120323     C                   z-add     blpvlf        vabvlb
046800120323     C                   z-add     blpqft        vabqft
046900120323     C                   z-add     blprmn        vabrmn
047000120323     C                   z-add     blpncd        vabncd
047100120323     C                   z-add     blpnca        vabnca
047200120323     C                   z-add     blpznc        vabznc
047300120323     C                   z-add     blpdbr        vabdcr
047400120326     C                   z-add     blpIAS        vabIAS
047500120323     C                   movel     blpVAS        vabVAS
047600120323     C                   movel     blpXCO        vabXCO
047700120323     C                   movel     blpRMA        vabRMA
047800130417     C                   movel     blpRMO        vabRMO
047900120326     C                   movel     blpSCL        vabSCL
048000001227     C* Considerazioni per bolle "autogenerate"
048100120323     C                   if        blpSOP = '&' and
048200120326     C                             vabSCL = *blanks
048300001227     C                   movel     'A'           vabSCL
048400001227     C                   endif
048500001227     C*
048600001227     C                   exsr      REPCSB
048700081006     C                   exsr      REPDEST
048800001218     C*
048900010131     C                   if        *in55 = *off
049000050825     C   25              WRITE     FNVAB000
049100050825     C   26              exsr      WRITIVGD
049200010131     C                   endif
049300001218     C*
049400001218     C                   ENDSR
049500001221     C*------------------------------------------------------------------------*
049600050825
049700050825
049800050825
049900050825     C*------------------------------------------------------------------------*
050000050825     C* WRITIVGD - Routine di scrittura file TIVGD (file VAS generico download)
050100050825     C*------------------------------------------------------------------------*
050200050825     C     WRITIVGD      BEGSR
050300161205     C*
050400161206     C                   clear                   tivgd000
050500161205     C                   eval      vgdDTA = %subst(FNVABDS:1:%size(FNVABDS))
050600161205     C                   eval      vgdTIP = TIPFILE
050700170227     C                   eval      vgdKSU = '0'+%editc(CODCLIVAS:'X')
050800161205     C                   eval      vgdTSC = 'WW'
050900161205     C                   eval      vgdDAT = datcor
051000170227     C                   eval      vgdPRG = o§7VASPRG
051100170227     C                   eval      vgdPGM = procname
051200170227     C                   eval      vgdSTO = '?'
051300161206     C                   write     tivgd000
051400050825     C*
051500050825     C                   ENDSR
051600050825     C*------------------------------------------------------------------------*
051700001218
051800001218
051900001218
052000001218     C*------------------------------------------------------------------------*
052100120323     C* REPDEST - Routine di reperimento dati relativi al destinatario della bolla (FIAR4)
052200120326     C*           ... e ai dati del mittente originale
052300001218     C*------------------------------------------------------------------------*
052400001218     C     REPDEST       BEGSR
052500120323     C*
052600120323     C                   eval      ar4TRC = 'D'
052700120323     C     KeyAR401_C    chain     fiar401l
052800120323     C                   if        %found(fiar401l)
052900120323     C                   eval      vabRD2 = ar4NOT
053000001218     C                   endif
053100001218     C*
053200001218     C                   ENDSR
053300001218     C*------------------------------------------------------------------------*
053400001218
053500001218
053600001218
053700001218     C*------------------------------------------------------------------------*
053800120323     C* REPCSB - Routine di reperimento dati relativi al contrassegno
053900001218     C*------------------------------------------------------------------------*
054000001218     C     REPCSB        BEGSR
054100001218     C*
054200120323     C     KCSB          chain     fiar901l
054300120323     C                   if        %found(fiar901l)
054400120323     C                   movel     ar9TIC        vabTIC
054500120323     C                   z-add     ar9CAS        vabCAS
054600120323     C                   movel     ar9VCA        vabVCA
054700120323     C                   movel     ar9GCA        vabGCA
054800001218     C                   endif
054900001218     C*
055000001218     C                   ENDSR
055100001218     C*------------------------------------------------------------------------*
055200010605
055300001218
055400001218
055500001218     C*------------------------------------------------------------------------*
055600001218     C* CARTBL - Routine di caricamento dati tabellati
055700001218     C*------------------------------------------------------------------------*
055800001218     C     CARTBL        BEGSR
055900001218     C*
056000001221     C                   Z-ADD     0             I                 4 0
056100050825     C                   IF        FLGUNI = 'S'
056200010301     C                   ADD       1             I
056300050825     C                   MOVEL     CODCLI        KUNI(I)
056400010301     C                   ELSE
056500001221     C                   MOVEL     '3Q'          COD
056600001218     C     KTAB          CHAIN     TABEL                              31
056700001218     C     *IN31         DOWEQ     '0'
056800090518     C                   IF        TBLFLG = ' '
056900090518     C***                IF        TBLFLG = ' ' AND
057000090518     C***                          %subst(TBLKEY:8:1) = ' '
057100001222     C                   MOVEL     TBLUNI        DS3Q
057200050825     C     §3QCKS        IFEQ      CODCLI
057300001221     C                   ADD       1             I
057400001221     C                   MOVEL     TBLKEY        KUNI(I)
057500001221     C                   ENDIF
057600001218     C                   ENDIF
057700001218     C     KTAB          READE     TABEL                                  31
057800001218     C                   ENDDO
057900010301     C                   ENDIF
058000120326     C*
058100120326     C* Carico schiera tipi bolla nn validi ai fini della spedizione
058200120326     C                   CLEAR                   K
058300120326     C                   MOVEL     '3A'          cod
058400120326     C     ktab          SETLL     tabel00f
058500120326     C     ktab          READE     tabel00f
058600120326     C                   DOW       not %eof(tabel00f)
058700120326     C                   IF        tblflg<>'*'
058800120326     C                   MOVEL     tbluni        ds3A
058900120326     C                   IF        §3arbl='R' or
059000120326     C                             §3arbl='C'
059100120326     C                   ELSE
059200090217     C                   ADD       1             K
059300120326     C                   MOVEL     TBLKEY        CBO(K)
059400120326     C                   MOVEL     §3ATB1        TBL(K)
059500120326     C                   ENDIF
059600120326     C                   ENDIF
059700120326     C     ktab          READE     tabel00f
059800120326     C                   ENDDO
059900120326     C
060000001218     C*
060100001218     C                   ENDSR
060200001221     C*------------------------------------------------------------------------*
060300161205
060400161205
060500161205
060600161205     C*------------------------------------------------------------------------*
060700161205     C* EXEERR - Routine di esecuzione azioni su Errore
060800161205     C*------------------------------------------------------------------------*
060900161205     C     EXEERR        BEGSR
061000161205     C*
061100161205     C                   dump(A)
061200161205     C                   rolbk
061300161205     C                   seton                                        lr
061400161205     C                   return
061500161205     C*
061600161205     C                   ENDSR
061700161205     C*------------------------------------------------------------------------*
061800001218
061900001218
062000001218
062100001218     C*------------------------------------------------------------------------*
062200001218     C* *INZSR - ROUTINE INIZIALE
062300001218     C*------------------------------------------------------------------------*
062400001218     C     *INZSR        BEGSR
062500001218     C*
062600001218     C     *ENTRY        PLIST
062700100312     C                   PARM                    PARAM            37
062800001218     C*
062900001218     C                   MOVEL     PARAM         DSINPUT
063000001218     C                   Z-ADD     1             CODUT
063100001218     C*
063200001218     C     KCSB          KLIST
063300120323     C                   KFLD                    blpAAS
063400120323     C                   KFLD                    blpLNP
063500120323     C                   KFLD                    blpNRS
063600120323     C                   KFLD                    blpNSP
063700001218     C*
063800001218     C     KTAB          KLIST
063900001218     C                   KFLD                    CODUT             1 0
064000001218     C                   KFLD                    COD               2
064100120323     C*
064200120323     C* Chiave su FIAR401L - Completa
064300120323     C     KeyAR401_C    klist
064400120323     C                   kfld                    blpAAS
064500120323     C                   kfld                    blpLNP
064600120323     C                   kfld                    blpNRS
064700120323     C                   kfld                    blpNSP
064800120323     C                   kfld                    ar4TRC
064900001218     C*
065000001218     C* Determino la data corrente
065100100312     C                   Z-ADD     *zeros        DATCOR            8 0
065200100312     C                   EVAL      DATCOR = %dec(%date() : *ISO)
065300001218     C*
065400001218     C                   ENDSR
