000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200010509     H*-------------------------------------------------------------------------*
000300010509     H*         - RITORNO DATI AL CLIENTE (ACCETTAZIONE BOLLE/ESISTI CONSEGNA)
000400010509     H*-------------------------------------------------------------------------*
000500010726     FRECLAMIPT IF   E             DISK    rename(ncdptlost6:ncdptlost0)
000600040402     F                                     prefix(C_)
000700040402     FFNVACPTDBRIF   E             DISK
000800040402     FFNVACPTDBOUF A E             DISK    rename(fnvacdbr:fnvacdbro)
000900040402     F                                     prefix(O_)
001000010509     FTABEL00F  IF   E           K DISK
001100010509     FTITAS34C  IF   E           K DISK
001200010131     F                                     rename(titas000:titasx00)
001300010131     F                                     rename(titas010:titasx10)
001400010131     F                                     rename(titasP00:titasxP0)
001500001218     FTITA430C  IF   E           K DISK
001600001218     FTITAA30C  IF   E           K DISK
001700001221     FTNCSB03L  IF   E           K DISK
001800010509     FFNEVB01L  IF   E           K DISK
001900010626     FFNDCT02L  IF   E           K DISK
002000010726     FFNVABPTX  UF A E             DISK
002100010726     FFNVACPT   UF A E           K DISK
002200010726     FFNVACPTL  IF   E           K DISK    rename(fnvac000:fnvac00l)
002300050309     FFNVACPT3L IF   E           K DISK    rename(fnvac000:fnvac03l)
002400050309     F                                     prefix(NOTAS_)
002500010509     FQSYSPRT   O    F  132        PRINTER
002600001218     D*--------------------
002700941228     D* PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
002800001218     D*--------------------
002900941228     D WLBDAT          DS                  INZ
003000941228     D  G02DAT                 1      8  0
003100941228     D  G02INV                 9     16  0
003200941228     D  G02ERR                17     17
003300941228     D  G02TGI                18     22  0
003400001218     D*--------------------
003500001218     D* DS ESTERNE
003600001218     D*--------------------
003700900517     D KPJBA         E DS
003800010509     D DS2A          E DS
003900000526     D DSBL4J        E DS
004000010606     D*--------------------
004100010606     D* DS DI RIDEFINIZIOINE RECORD DA XLS POSTE
004200010606     D*--------------------
004300010606     D                 DS
004400010626     D  SEGN                         12
004500010925     D  DATACORRI                     8
004600010925     D  DATA1TENT                     8
004700010626     D  NOMEFILE                     50
004800021022     D  RECORDFILE                 1008
004900010626     D  DSRECPT                1   1086
005000021004     D*--------------------
005100021004     D* DS DI RIDEFINIZIOINE RECORD DA FILE "DATABASE RECLAMI" POSTE
005200021004     D*--------------------
005300021004     D                 DS
005400021004     D  IN_LDV                       13
005500021022     D  RAGSOCMITT                   40
005600021022     D  DSRECPTDBR             1     53
005700001218     D*--------------------
005800010509     D* DS DI RIDEFINIZIOINE SEGNACOLLO POSTE
005900001218     D*--------------------
006000010925     D                 DS
006100010925     D  FLS                           3  0
006200010925     D  NRS                           2  0
006300010925     D  NCD                           7  0
006400010925     D  DSNCDPT                1     12
006500010925     D*--------------------
006600011112     D* DS DI RIDEFINIZIOINE CAMPO DATA SPEDIZIONE VAC
006700010925     D*--------------------
006800010925     D                 DS
006900010925     D  VACAAS                        4  0
007000010925     D  VACMGS                        4  0
007100010925     D  VACDSP                 1      8  0
007200011112     D*--------------------
007300011112     D* DS DI RIDEFINIZIOINE CAMPO DATA SPEDIZIONE TAS
007400011112     D*--------------------
007500011112     D                 DS
007600011112     D  TASAAS                        4  0
007700011112     D  TASMGS                        4  0
007800011112     D  TASDSP                 1      8  0
007900010925     D*--------------------
008000010925     D* DS RECUPERO GG LAVORATIVI TRA DUE DATE
008100010925     D*--------------------
008200010925     D  WDAT8          DS
008300010925     D   DATADA                       8  0
008400010925     D   DATAA                        8  0
008500010925     D   GGL                          5  0
008600010509     D*--------------------
008700010509     D* SCHIERA PER CARICAMENTO CODICI E TIPI MANCATE CONSEGNE
008800010509     D*--------------------
008900010509     D C2A             S              3    DIM(300)                             cd.manc.cons.
009000010509     D T2A             S              1    DIM(300)                             tp.manc.cons.
009100010925     D*--------------------
009200010925     D* VARIABILI DI WRK
009300010925     D*--------------------
009400010925     D MULT            S              1  0 INZ(*zeros)
009500021028     D*-------------------
009600021028     D* CAMBIO DIVISA
009700021028     D*-------------------
009800021028     D  YEUR         E DS                  EXTNAME(YEURCODS)
009900030304     D*-------------------
010000030304     D* VARIABILI DI WRK
010100030304     D*-------------------
010200040402     D PT_DSINPUT    E DS                  EXTNAME(FNVACPTDBR)
010300040402     D PT_DSOUTPUT   E DS                  EXTNAME(FNVACPTDBO) PREFIX(O_)
010400040625     D*-------------------
010500040625     D* COSTANTI
010600040625     D*-------------------
010700040625     D ConstNUM        C                   CONST('1234567890')
010800010606     C*---------------------------------------------------------------*
010900001218
011000001218
011100001218
011200920812     C*---------------------------------------------------------------*
011300001218     C* MAIN
011400001218     C*---------------------------------------------------------------*
011500010509     C                   exsr      cartbl
011600001218     C                   exsr      procedi
011700001218     C*
011800001218     C                   seton                                        LR
011900010509     C*---------------------------------------------------------------*
012000001218
012100001218
012200001218
012300001218     C*------------------------------------------------------------------------*
012400001218     C* PROCEDI - Routine principale
012500001218     C*------------------------------------------------------------------------*
012600001218     C     PROCEDI       BEGSR
012700021004     C*
012800021004     C* Innanzitutto inizializzo le DS di wrk
012900021004     C                   clear                   DSRECPT
013000021004     C                   clear                   DSRECPTDBR
013100021115     C*
013200021115     C* Inizializzo il flag di fine elaborazione
013300021115     C                   movel     '0'           $fine             1
013400021004     C*
013500021004     C* A seconda del tipo di richiesta nel lancio leggo o il file RECLAMIPT "originale"
013600021004     C* o il file FNVACPTDBR "nuovo"
013700021004     C                   if        TIN = '1'
013800001218     C*
013900010509     C* Leggo sequenzialmente tutto il file di input "Segnacolli Lost SDA"
014000010726     C                   read(e)   RECLAMIPT
014100021004     C                   if        %eof(RECLAMIPT)
014200021004     C                   movel     '1'           $fine
014300021004     C                   endif
014400010719     C                   movel(p)  c_ncdptlost6  DSRECPT
014500040625     C                   movel(p)  SEGN          DSNCDPT
014600010627     C* Verifico x sicurezza la correttezza del segnacollo
014700010627     C                   exsr      CHKNCD
014800021004     C                   endif
014900021004     C*
015000021004     C                   if        TIN = '2'
015100021004     C*
015200021004     C* Leggo sequenzialmente tutto il file di input "DATABASE RECLAMI" POSTE
015300021004     C                   read(e)   FNVACPTDBR
015400040405     C*
015500040405     C* Verifico se il primo record contiene l'intestazione dei campi
015600040405     C                   if        %subst(dbrLDV:1:3) = '180'
015700040405     C                   else
015800040405     C                   read(e)   FNVACPTDBR
015900040405     C                   endif
016000040405     C*
016100021004     C                   if        %eof(FNVACPTDBR)
016200021004     C                   movel     '1'           $fine
016300021004     C                   endif
016400021022     C                   movel(p)  DBRLDV        IN_LDV
016500021022     C                   movel(p)  DBRRSM        RAGSOCMITT
016600040625     C                   movel(p)  IN_LDV        DSNCDPT
016700021004     C* Verifico x sicurezza la correttezza del segnacollo
016800021004     C                   exsr      CHKNCD
016900021004     C                   endif
017000010509     C*
017100021004     C                   dow       $fine = '0'
017200040209     C*
017300040209     C* Azzero il contatore dei segnacolli PT
017400040209     C                   z-add     *zeros        countNCD         10 0
017500050309     C*
017600050309     C* Azzero variabili d wrk
017700050309     C                   movel     *blanks       Chk_SCX           1
017800050309     C                   movel     *blanks       Val_CCA           1
017900050310     C                   movel     *blanks       Val_IDL           5
018000051109     C                   movel     *blanks       Val_OLD           1
018100010509     C*
018200030325     C                   if        ((DSRECPT    <> *blanks AND TIN = '1')    OR
018300040408     C                              (DSRECPTDBR <> *blanks AND TIN = '2'))
018400040408     C***                           (DSRECPTDBR <> *blanks AND TIN = '2'))   AND
018500040408     C***                          flgNnTro = '0'
018600040408     C*
018700040408     C* Se segnacollo formalmente corretto aggancio la bolla altrimenti scrivo subito l'output
018800040408     C                   if        flgNnTro = '0'
018900001218     C*
019000010509     C* Mi posiziono sulle bolle e ciclo x ugual segnacollo
019100011203     C*
019200011203     C* Verifico che il segnacollo da file input POSTE non contenga *blanks
019300021004     C                   if        (%scan('+':SEGN)   = 0 AND TIN = '1') OR
019400021004     C                             (%scan('+':IN_LDV) = 0 AND TIN = '2')
019500011203     C*
019600021118     C* Se tipo input = '2'
019700021118     C                   if        TIN = '2'
019800040402     C* Eseguo un primo giro d lettura x stabilire lo stato da atribuire al segnacollo
019900021118     C* (anche se doppio)
020000021118     C     K34C          chain     titas34c                           55
020100021118     C                   z-add     *zeros        countNCD_SAV     10 0
020200021118     C                   dow       not *in55
020300021118     C                   add       1             countNCD_SAV
020400021118     C     K34C          reade     titas34c                               55
020500021118     C                   enddo
020600021119     C                   endif
020700021118     C*
020800021118     C* Eseguo un secondo giro x scrivere effettivamente
020900010927     C     K34C          chain     titas34c                           55
021000001218     C*
021100011112     C                   dow       not *in55
021200011203     C*                  if        tasDSP < 20010401
021300011203     C*                  else
021400020201     C                   if        DATACORRI > *blanks
021500020201     C                   movel(p)  DATACORRI     WrkAnno           4 0
021600020201     C                   setoff                                       54
021700020201     C                   if        tasaas = WrkAnno
021800020201     C                   else
021900020201     C                   seton                                        54
022000020201     C                   endif
022100020201     C                   endif
022200021118     C                   add       1             countNCD
022300010627     C                   exsr      INZFIL
022400040402     C                   exsr      INZDBR
022500010509     C                   exsr      exeVABVAC
022600001221     C                   exsr      scriviVAB
022700010509     C                   exsr      scriviVAC
022800011203     C*                  endif
022900010509     C*
023000010927     C     K34C          reade     titas34c                               55
023100001218     C                   enddo
023200011203     C*
023300011203     C                   endif
023400011203     C*
023500010509     C* ... se non trovo nessuna bolla con quel segnacollo => listo
023600040209     C* ... o segnacollo formalmente scorretto           o => listo
023700040209     C                   if        countNCD = *zeros or flgNnTro <> '0'
023800010627     C                   exsr      INZFIL
023900040402     C                   exsr      INZDBR
024000010626     C                   exsr      WRIVAC
024100010509     C                   endif
024200011203     C*
024300040408     C                   else
024400040408     C                   exsr      INZFIL
024500040408     C                   exsr      INZDBR
024600040408     C                   exsr      WRIVAC
024700040408     C                   endif
024800011203     C                   endif
024900010509     C*
025000021004     C                   if        TIN = '1'
025100010726     C                   read(e)   RECLAMIPT
025200021004     C                   if        %eof(RECLAMIPT)
025300021004     C                   movel     '1'           $fine
025400021004     C                   endif
025500010626     C                   movel(p)  c_ncdptlost6  DSRECPT
025600040625     C                   movel(p)  SEGN          DSNCDPT
025700021004     C* Verifico x sicurezza la correttezza del segnacollo
025800021004     C                   exsr      CHKNCD
025900021004     C                   endif
026000021004     C                   if        TIN = '2'
026100021004     C                   read(e)   FNVACPTDBR
026200040408     C* Verifico se il primo record contiene l'intestazione dei campi
026300040408     C                   if        %subst(dbrLDV:1:3) = '180'
026400040408     C                   else
026500040408     C                   read(e)   FNVACPTDBR
026600040408     C                   endif
026700040408     C*
026800021004     C                   if        %eof(FNVACPTDBR)
026900021004     C                   movel     '1'           $fine
027000021004     C                   endif
027100021022     C                   movel(p)  DBRLDV        IN_LDV
027200021022     C                   movel(p)  DBRRSM        RAGSOCMITT
027300040625     C                   movel(p)  IN_LDV        DSNCDPT
027400021004     C* Verifico x sicurezza la correttezza del segnacollo
027500021004     C                   exsr      CHKNCD
027600021004     C                   endif
027700001218     C                   enddo
027800001218     C*
027900001218     C                   ENDSR
028000001218     C*------------------------------------------------------------------------*
028100010627
028200010627
028300010627
028400010627     C*------------------------------------------------------------------------*
028500010627     C* CHKNCD - Routine di verifica correttezza segnacollo BAR
028600010627     C*------------------------------------------------------------------------*
028700010627     C     CHKNCD        BEGSR
028800030325     C*
028900030325     C                   movel     '0'           flgNnTro          1
029000010627     C*
029100040625     C* Verifico se la lunghezza è corretta
029200021004     C                   if        TIN = '1'
029300040625     C                   if        %len(%trim(SEGN)) < 12
029400030325     C                   movel     '1'           flgNnTro
029500030325     C***                dow       %len(%trim(SEGN))   < 12
029600030325     C***                eval      SEGN = %subst(SEGN:1:5) + '0' +
029700030325     C***                          %subst(SEGN:6:(%len(%trim(SEGN))-6+1))
029800030325     C***                enddo
029900021004     C                   endif
030000030325     C                   endif
030100040625     C*
030200040625     C* Verifico se la lunghezza è corretta
030300021004     C                   if        TIN = '2'
030400040625     C                   if        %len(%trim(IN_LDV)) < 12
030500030325     C                   movel     '1'           flgNnTro
030600030325     C***                dow       %len(%trim(IN_LDV)) < 12
030700030325     C***                eval      IN_LDV = %subst(IN_LDV:1:5) + '0' +
030800030325     C***                          %subst(IN_LDV:6:(%len(%trim(IN_LDV))-6+1))
030900030325     C***                enddo
031000021004     C                   endif
031100030325     C                   endif
031200010627     C*
031300040625     C* Se la lunghezza è corretta in ogni caso verifico che sia numerico
031400040625     C                   movel     DSNCDPT       wStrNum          30
031500040625     C                   exsr      CHKNUM
031600040625     C                   if        wStrNumFlg = '0'
031700040625     C                   movel     '1'           flgNnTro
031800040625     C                   endif
031900040625     C*
032000010627     C                   ENDSR
032100010627     C*------------------------------------------------------------------------*
032200001218
032300001218
032400010509
032500010627     C*------------------------------------------------------------------------*
032600010627     C* INZFIL - Routine di inizializzazione formati record file di OUTPUT
032700010627     C*------------------------------------------------------------------------*
032800010627     C     INZFIL        BEGSR
032900010627     C*
033000010627     C                   clear                   fnvab000
033100010627     C                   clear                   fnvac000
033200010627     C*
033300010627     C                   ENDSR
033400010627     C*------------------------------------------------------------------------*
033500040402
033600040402
033700040402
033800040402     C*------------------------------------------------------------------------*
033900040402     C* INZDBR - Routine di inizializzazione formati record file di OUTPUT reclami standard
034000040402     C*------------------------------------------------------------------------*
034100040402     C     INZDBR        BEGSR
034200040402     C*
034300040402     C* Valorizzo qui i campi del file reclami standard FNVACPTDBO
034400040402     C                   clear                   fnvacdbro
034500040402     C*
034600040402     C* D default imposto i campi dell'output da quelli d input
034700040402     C                   eval      PT_DSOUTPUT = PT_DSINPUT
034800040402     C*
034900040402     C                   ENDSR
035000040402     C*------------------------------------------------------------------------*
035100010627
035200010627
035300010627
035400010509     C*------------------------------------------------------------------------*
035500010509     C* EXEVABVAC - Routine di inizializzazione e reperimento dati comuni VAB/VAC
035600010509     C*------------------------------------------------------------------------*
035700010509     C     EXEVABVAC     BEGSR
035800010509     C*
035900010509     C* Effettuo considerazioni particolare per bolla "POSTE"
036000010509     C     tasTSP        ifeq      'P'                                          BOLLA POSTE
036100010509     C                   exsr      REPPTIAS
036200010509     C                   exsr      REPPTGIA
036300010509     C                   z-add     tasNCD        vacDCR
036400010509     C                   endif
036500010509     C*
036600010509     C* Reperisco i dati relativi al contrassegno
036700010509     C                   exsr      REPCSB
036800010626     C*
036900010626     C* Reperisco i dati relativi alla Lettera di Anomalia
037000010626     C                   exsr      REPLA
037100010509     C*
037200010509     C* Reperisco i dati relativi al riferimento mittente alfabetico
037300010509     C                   exsr      REPRMA
037400010509     C*
037500010509     C* Reperisco dall'estensione anagrafica bolle di SEDE i dati del Destinatario
037600010509     C                   exsr      REPDEST
037700010509     C*
037800010509     C* Reperisco i dati relativi al lasciato avviso
037900010509     C                   exsr      REPLAV
038000010509     C*
038100010509     C                   ENDSR
038200010509     C*------------------------------------------------------------------------*
038300010131
038400001218
038500001218
038600001218     C*------------------------------------------------------------------------*
038700001221     C* SCRIVIVAB - Routine di scrittura file esiti di consegna (FNVAB)
038800001218     C*------------------------------------------------------------------------*
038900001221     C     SCRIVIVAB     BEGSR
039000001218     C*
039100001221     C                   movel     tascbo        vabcbo
039200001221     C                   movel     tasrsd        vabrsd
039300001221     C                   movel     tasind        vabind
039400001221     C                   movel     tascad        vabcad
039500001221     C                   movel     taslod        vablod
039600001221     C                   movel     tasprd        vabprd
039700001221     C                   movel     tasnzd        vabnzd
039800001221     C                   movel     tasgc1        vabgc1
039900001221     C                   movel     tasgc2        vabgc2
040000001221     C                   movel     tastsp        vabtsp
040100001221     C                   movel     tasctm        vabctm
040200001221     C                   movel     tasffd        vabffd
040300001221     C                   movel     tastcr        vabtcr
040400001221     C                   movel     tascts        vabcts
040500001221     C                   movel     tasftm        vabftm
040600001221     C                   movel     tasgma        vabgma
040700001221     C                   movel     tasgga        vabgga
040800001221     C                   movel     tasgva        vabgva
040900010111     C                   move      ' '           vabgva
041000001221     C                   if        tasccm > 0
041100001221     C                   z-add     tasccm        vabccm
041200001221     C                   else
041300001221     C                   z-add     tasksc        vabccm
041400001221     C                   end
041500001221     C                   z-add     taslnp        vablnp
041600001221     C                   z-add     tasaas        vabaas
041700001221     C                   z-add     tasmgs        vabmgs
041800001221     C                   z-add     tasnrs        vabnrs
041900001221     C                   z-add     tasnsp        vabnsp
042000001221     C                   z-add     taslna        vablna
042100001221     C                   z-add     tasctr        vabctr
042200001221     C                   z-add     tasncl        vabncl
042300001221     C                   z-add     taspkf        vabpkb
042400001221     C                   z-add     tasvlf        vabvlb
042500001221     C                   z-add     tasqft        vabqft
042600001221     C                   z-add     tasrmn        vabrmn
042700001221     C                   z-add     tasncd        vabncd
042800001221     C                   z-add     tasnca        vabnca
042900001221     C                   z-add     tasznc        vabznc
043000010108     C                   z-add     tasdbr        vabdcr
043100001227     C* Considerazioni per bolle "autogenerate"
043200001227     C                   if        tasSOP = '&' and
043300001227     C                             vabSCL = *blanks
043400001227     C                   movel     'A'           vabSCL
043500001227     C                   endif
043600001218     C*
043700001227     C                   write     FNVAB000
043800001218     C*
043900001218     C                   ENDSR
044000001221     C*------------------------------------------------------------------------*
044100001218
044200001221
044300010509
044400010509     C*------------------------------------------------------------------------*
044500010509     C* SCRIVIVAC - Routine di scrittura file esiti di consegna (FNVAC)
044600010509     C*------------------------------------------------------------------------*
044700010509     C     SCRIVIVAC     BEGSR
044800010509     C*
044900010509     C                   MOVEL     tasAAS        VACAAS
045000010509     C                   MOVEL     tasLNP        VACLNP
045100010509     C                   MOVEL     tasNRS        VACNRS
045200010509     C                   MOVEL     tasNSP        VACNSP
045300010509     C                   MOVEL     tasMGS        VACMGS
045400010509     C                   MOVEL     tasCBO        VACCBO
045500010509     C                   MOVEL     tasLNA        VACLNA
045600010509     C                   MOVEL     tasRSD        VACRSD
045700010509     C                   MOVEL     tasPRD        VACPRD
045800010509     C                   MOVEL     tasGC1        VACGC1
045900010509     C                   MOVEL     tasGC2        VACGC2
046000010509     C                   MOVEL     tasCTR        VACCTR
046100010509     C                   MOVEL     tasCTS        VACCTS
046200010509     C                   MOVEL     tasFTM        VACFTM
046300010509     C                   MOVEL     tasFIN        VACFIN
046400010509     C                   MOVEL     tasFAP        VACFAP
046500010509     C                   MOVEL     tasTSP        VACTSP
046600010509     C                   MOVEL     tasNCL        VACNCL
046700010509     C                   MOVEL     tasPKF        VACPKB
046800010509     C                   MOVEL     tasVLF        VACVLB
046900010509     C                   MOVEL     tasQFT        VACQFT
047000010509     C     tasCCM        IFNE      0
047100010509     C                   MOVEL     tasCCM        VACCCM
047200010509     C                   ELSE
047300010509     C                   MOVEL     tasKSC        VACCCM
047400010509     C                   ENDIF
047500010509     C                   MOVEL     tasRMN        VACRMN
047600010509     C                   MOVEL     tasFFD        VACFFD
047700010509     C                   MOVEL     tasTCR        VACTCR
047800010605     C*                  MOVEL     tasHCR        VACHCR
047900010605     C                   Z-ADD     tasNRC        VACHCR
048000010509     C                   MOVEL     tasFTC        VACTC1
048100010509     C                   MOVEL     tasTC2        VACTC2
048200010509     C                   MOVEL     tasDCM        VACDCM
048300010509     C                   MOVEL     tasHMC        VACHMC
048400010509     C*
048500040402     C*                  IF        tasTSP = 'P' and tasCCA = '7'
048600010509     C*                  MOVEL     *blanks       VACCCA
048700010509     C*                  ELSE
048800010509     C                   MOVEL     tasCCA        VACCCA
048900010509     C*                  ENDIF
049000010626     C*
049100010726     C* Verifico eventuale doppio input all'interno dell'invio attuale
049200010726     C     KVACL         SETLL     FNVACPTL                               77
049300010726     C*
049400010726     C* Verifico eventuale doppio input all'interno di tutti gli invii
049500010726     C     KVAC          SETLL     FNVACPT                                78
049600010726     C*
049700010627     C                   IF        *in77
049800010626     C                   EVAL      VACXX4 = '1'
049900010626     C                   ENDIF
050000040206     C                   IF        *in78 AND DOPPI = 'S'
050100010726     C                   EVAL      VACXX5 = '1'
050200010726     C                   ENDIF
050300020201     C                   IF        *in54
050400020201     C                   EVAL      VACXX9 = '1'
050500020201     C                   ENDIF
050600010626     C                   EXSR      WRIVAC
050700010509     C*
050800010509     C                   ENDSR
050900010509     C*------------------------------------------------------------------------*
051000010626
051100010626
051200010626
051300010626     C*------------------------------------------------------------------------*
051400010626     C* WRIVAC - Routine di scrittura file FNVAC
051500010626     C*------------------------------------------------------------------------*
051600010626     C     WRIVAC        BEGSR
051700010626     C*
051800010726     C* Considerazioni x doppio segnacollo in bolle BAR
051900010626     C                   IF        countNCD > 1
052000010626     C                   EVAL      VACXX3 = '1'
052100010626     C                   ENDIF
052200010726     C*
052300010726     C* Considerazioni x segnacollo non trovato in bolle BAR
052400010626     C                   IF        countNCD = 0
052500010626     C                   EVAL      VACXX1 = '1'
052600010626     C                   ENDIF
052700010626     C*
052800010626     C* Imposto i campi riportanti i dati dei file dei clienti
052900021004     C                   IF        TIN = '1'
053000010626     C                   EVAL      VACSCX = SEGN
053100021004     C                   ENDIF
053200021004     C                   IF        TIN = '2'
053300021004     C                   EVAL      VACSCX = IN_LDV
053400021004     C                   ENDIF
053500010925     C                   IF        DATACORRI <> *blanks
053600010925     C                   MOVEL     DATACORRI     VACDCX
053700010925     C                   ENDIF
053800010925     C                   IF        DATA1TENT <> *blanks
053900010925     C                   MOVEL     DATA1TENT     VACD1X
054000010925     C                   ENDIF
054100021022     C                   IF        TIN = '1'
054200021022     C                   EVAL      VACNFI = %trim(NOMEFILE)
054300010626     C                   EVAL      VACREC = RECORDFILE
054400021022     C                   ENDIF
054500021022     C                   IF        TIN = '2'
054600021022     C                   EVAL      VACNFI = %trim(RAGSOCMITT)
054700021022     C                   ENDIF
054800010925     C*
054900010927     C* Se lancio "Monitor" calcoro le ore di differenza tra le nostre e le loro date
055000010927     C                   IF        TPL = 'M'
055100010925     C                   CLEAR                   WDAT8
055200010925     C                   IF        VACDSP < VACDCX
055300010925     C                   EVAL      DATADA = VACDSP
055400010925     C                   EVAL      DATAA  = VACDCX
055500010925     C                   EVAL      MULT = 1
055600010925     C                   ELSE
055700010925     C                   EVAL      DATADA = VACDCX
055800010925     C                   EVAL      DATAA  = VACDSP
055900010925     C                   EVAL      MULT = -1
056000010925     C                   ENDIF
056100010925     C                   CALL      'XSRLAV8'
056200010925     C                   PARM                    WDAT8
056300010927     C                   IF        GGL > 41
056400010927     C                   EVAL      VACHH6 = 999
056500010927     C                   ELSE
056600010925     C                   EVAL      VACHH6 = %abs(GGL-1) * 24 * MULT
056700010927     C                   ENDIF
056800010927     C                   ENDIF
056900010726     C*
057000010726     C* Imposto i campi riportanti valori costanti
057100030304     C                   EVAL      VACIDL = IDL
057200030304     C*
057300030304     C* Verifico "l'anzianità" della bolla: se + vecchia della data presa in input
057400030304     C* => ignoro (o meglio conteggio a parte)
057500030304     C                   IF        VACDSP > *zeros AND
057600030304     C                             VACDSP < numDAT
057700030304     C                   EVAL      VACXX8 = '1'
057800030304     C                   ENDIF
057900050310     C*
058000050310     C* Se richiesta nei parametri d lancio la verifica x eventuale bolla già
058100050310     C* cancellata in TITAS provo a verificare se nel passato tale segnacollo
058200050310     C* era già stato elaborato (ovviamente trovando la bolla)
058300050310     C                   IF        NOTAS <> *blanks
058400050310     C                   MOVEL(P)  DSNCDPT       KeySCX           12
058500050310     C     KeySCX        CHAIN     FNVACPT3L
058600050310     C                   IF        %found(FNVACPT3L)
058700050310     C                   EVAL      Chk_SCX = 'S'
058800050310     C                   EVAL      Val_CCA = NOTAS_vacCCA
058900050310     C                   EVAL      Val_IDL = NOTAS_vacIDL
059000051109     C                   EVAL      Val_OLD = NOTAS_vacXX8
059100050310     C                   ENDIF
059200050310     C                   ENDIF
059300021118     C*
059400021118     C* Se tipo input = '2' effettuo alcune particolari considerazioni che influenzano
059500021118     C* la scrittura o meno del record in FNVACPT
059600021118     C                   IF        TIN = '2'
059700021118     C                   IF        countNCD_SAV > 1
059800021118     C                   EVAL      VACXX3 = '1'
059900021118     C                   ENDIF
060000020201     C                   WRITE     FNVAC000
060100021119     C                   EXSR      WRIVACDBR
060200021118     C                   ENDIF
060300010626     C*
060400010626     C                   ENDSR
060500010626     C*------------------------------------------------------------------------*
060600021115
060700021115
060800021115
060900021115     C*------------------------------------------------------------------------*
061000021115     C* WRIVACDBR - Routine di scrittura file FNVACDBR
061100021115     C*------------------------------------------------------------------------*
061200021115     C     WRIVACDBR     BEGSR
061300021115     C*
061400021115     C* Per tipo input = '2' aggiorno anche il file d input stesso
061500021115     C                   if        TIN = '2'
061600040402     C*
061700040402     C* Se nn trovato segnacollo lascio intatti i campi d input altrimenti passo quelli
061800040402     C* presenti sulla bolla Bartolini
061900040402     C                   if        countNCD = *zeros or flgNnTro <> '0'
062000040402     C                   else
062100040402     C                   eval      O_dbrRSD = tasRSD
062200040402     C                   eval      O_dbrIND = tasIND
062300040402     C                   eval      O_dbrLOD = tasLOD
062400040402     C                   eval      O_dbrPRD = tasPRD
062500040402     C                   eval      O_dbrCAD = tasCAD
062600040402     C                   endif
062700021115     C*
062800021115     C* Inizializzo i campi da aggiornare
062900040402     C                   clear                   O_dbrDSP
063000040402     C                   clear                   O_dbrDST
063100040402     C                   clear                   O_dbrSTS
063200040402     C                   clear                   O_dbrIMC
063300040402     C                   clear                   O_dbrDIC
063400021115     C*
063500021115     C                   movel     vacAAS        wrkDATAalfa       8
063600021115     C                   move      vacMGS        wrkDATAalfa
063700040402     C                   eval      O_dbrDSP = %subst(wrkDATAalfa:1:4) + '-' +
063800040402     C                                        %subst(wrkDATAalfa:5:2) + '-' +
063900040402     C                                        %subst(wrkDATAalfa:7:2)
064000021115     C                   movel     vacDCM        wrkDATAalfa
064100040402     C                   eval      O_dbrDST = %subst(wrkDATAalfa:1:4) + '-' +
064200040402     C                                        %subst(wrkDATAalfa:5:2) + '-' +
064300040402     C                                        %subst(wrkDATAalfa:7:2)
064400021115     C*
064500021115     C* Effettuo considerazioni x determinare lo status della spedizione
064600021115     C                   if        vacDCM > *zeros AND
064700021115     C                             (vacCCA = *blanks OR
064800021115     C                              vacCCA = '7')
064900040402     C                   eval      O_dbrSTS = 'OK'
065000021115     C                   else
065100021115     C                   if        vacXX1 = '1'
065200040402     C                   eval      O_dbrSTS = 'NT'
065300021115     C                   else
065400021115     C                   if        vacXX3 = '1'
065500040402     C                   eval      O_dbrSTS = 'DP'
065600021115     C                   else
065700040402     C                   eval      O_dbrSTS = vacCCA
065800021115     C                   endif
065900021115     C                   endif
066000030304     C                   endif
066100050310     C*
066200040206     C* Verifico se la spedizione è antecedente alla data limite indicata nei parametri d lancio
066300040206     C                   if        vacXX8 = '1'
066400040402     C                   eval      O_dbrOLD = 'O'
066500040402     C***                eval      O_dbrSTS = 'OLD'
066600040206     C***                movel     *zeros        wrkDATAalfa
066700040402     C***                eval      O_dbrDST = %subst(wrkDATAalfa:1:4) + '-' +
066800040402     C***                                     %subst(wrkDATAalfa:5:2) + '-' +
066900040402     C***                                     %subst(wrkDATAalfa:7:2)
067000040206     C                   endif
067100040206     C*
067200021115     C*
067300021115     C* A questo punto verifico se trattasi di reclamo DOPPIO ovvero se già in precedenza
067400021115     C* elaborato
067500021115     C                   if        vacXX4 <> *blanks OR
067600021115     C                             vacXX5 <> *blanks
067700040402     C                   eval      %subst(O_dbrSTS:3:1) = 'D'
067800021115     C                   endif
067900021115     C*
068000021115     C                   z-add     vacCAS        COMIMP
068100021115     C                   if        vacVCA <> 'EUR'
068200021115     C                   movel     vacVCA        COMDIV
068300021115     C                   exsr      CNVEUR
068400021115     C                   endif
068500021115     C                   z-add     COMIMP        wrkCAS           15 2
068600040402     C                   eval      O_dbrIMC = %trim(%editc(wrkCAS:'4'))
068700021115     C                   if        vacXX0 = '1'
068800021115     C                   movel     vacDDP        wrkDATAalfa
068900040402     C                   eval      O_dbrDIC = %subst(wrkDATAalfa:1:4) + '-' +
069000040402     C                                        %subst(wrkDATAalfa:5:2) + '-' +
069100040402     C                                        %subst(wrkDATAalfa:7:2)
069200021115     C                   endif
069300040402     C                   if        vacXX0 = '2' and O_dbrSTS = 'OK'
069400040402     C                   eval      O_dbrIMC = *all'0'
069500021115     C                   endif
069600021115     C*
069700021115     C* Se trattasi di "VEDI PACCO" => aggiorno la ragione sociale del file d INPUT
069800021115     C                   if        %trim(vacRSD) = 'VEDI PACCO'
069900040402     C                   eval      O_dbrRSD = vacRSD
070000021115     C                   endif
070100050310     C*
070200050310     C* In ultima istanza come "forzatura" se richiesto nel lancio il
070300050310     C* controllo particolare x "NOTAS" verifico se reperimento riuscito
070400050310     C                   if        NOTAS <> *blanks AND Chk_SCX = 'S'
070500050310     C                   eval      O_dbrOLD = 'T'
070600050310     C                   eval      O_dbrSTS = Val_CCA
070700050310     C                   eval      O_dbrCAM = Val_IDL
070800051109     C                   if        Val_OLD <> *blanks
070900051109     C                   eval      O_dbrOLD = 'H'
071000051109     C                   endif
071100050310     C                   endif
071200021115     C*
071300040402     C                   write     fnvacdbro
071400021115     C                   endif
071500021115     C*
071600021115     C                   ENDSR
071700021115     C*------------------------------------------------------------------------*
071800001221
071900010509
072000010509
072100010509     C*------------------------------------------------------------------------*
072200010509     C* REPPTGIA - Routine di reperimento dati relativi alla giacenza x POSTE
072300010509     C*------------------------------------------------------------------------*
072400010509     C     REPPTGIA      BEGSR
072500010509     C*
072600010509     C* Reperisco la data di giacenza dagli eventi poichè non vengono
072700010509     C* gestite le giacenze per le bolle poste
072800010509     C     KEVB          SETGT     FNEVB01L
072900010509     C     KEVB          READPE    FNEVB01L                               34
073000010509     C*
073100010509     C* Devo cercare l'ultimo evento di tipo giacenza
073200010509     C     *IN34         DOWEQ     *OFF
073300010509     C                   Z-ADD     1             W
073400010509     C     EVBCEV        LOOKUP    C2A(W)                                 42
073500010509     C     *IN42         IFEQ      '1'
073600010509     C     T2A(W)        ANDEQ     'G'
073700010509     C                   Z-ADD     EVBDEV        vacDAG
073800010509     C                   LEAVE
073900010509     C                   ELSE
074000010509     C     KEVB          READPE    FNEVB000                               34
074100010509     C                   ENDIF
074200010509     C                   ENDDO
074300010509     C*
074400010509     C                   ENDSR
074500010509     C*------------------------------------------------------------------------*
074600010509
074700010509
074800010509
074900010509     C*------------------------------------------------------------------------*
075000010509     C* REPLAV - Routine di reperimento dati relativi al lasciato avviso
075100010509     C*------------------------------------------------------------------------*
075200010509     C     REPLAV        BEGSR
075300010509     C*
075400010509     C* Pererisco l'ultimo evento Lasciato Avviso
075500010509     C                   MOVEL     '2A'          COD
075600010509     C     KEVB          SETGT     FNEVB01L
075700010509     C     KEVB          READPE    FNEVB01L                               34
075800010509     C*
075900010509     C     *IN34         DOWEQ     *OFF
076000010509     C                   MOVEL(P)  EVBCEV        KEY
076100010509     C     KTAB2         CHAIN     TABEL                              30
076200010509     C     *IN30         IFEQ      *OFF
076300010509     C                   MOVEL     TBLUNI        DS2A
076400010509     C     §2AFTC        IFEQ      'A'
076500010509     C                   Z-ADD     EVBDEV        vacDLA
076600010509     C                   LEAVE
076700010509     C                   ENDIF
076800010509     C                   ENDIF
076900010509     C     KEVB          READPE    FNEVB000                               34
077000010509     C                   ENDDO
077100010509     C*
077200010509     C                   ENDSR
077300010509     C*------------------------------------------------------------------------*
077400010509
077500010509
077600001218
077700010626     C*------------------------------------------------------------------------*
077800040402     C* IMPXLS - Routine di valorizzazoine campi provenienti da files XLS cleinti
077900010626     C*------------------------------------------------------------------------*
078000010626     C     IMPXLS        BEGSR
078100010626     C*
078200021004     C                   IF        TIN = '1'
078300010626     C                   EVAL      VACSCX = SEGN
078400021004     C                   ENDIF
078500021004     C                   IF        TIN = '2'
078600021004     C                   EVAL      VACSCX = IN_LDV
078700021004     C                   ENDIF
078800021022     C                   IF        TIN = '1'
078900021022     C                   EVAL      VACNFI = %trim(NOMEFILE)
079000021022     C                   EVAL      VACREC = RECORDFILE
079100021022     C                   ENDIF
079200021022     C                   IF        TIN = '2'
079300021022     C                   EVAL      VACNFI = %trim(RAGSOCMITT)
079400021022     C                   ENDIF
079500010626     C*
079600010626     C                   ENDSR
079700010626     C*------------------------------------------------------------------------*
079800010626
079900010626
080000010626
080100001218     C*------------------------------------------------------------------------*
080200001218     C* REPPTIAS - Routine di reperimento dati relativi all'importo/divisa da assicurare x POSTE
080300010131     C*          - e dei pacchi ingombranti non dichiarati
080400001218     C*------------------------------------------------------------------------*
080500001218     C     REPPTIAS      BEGSR
080600001218     C*
080700060215     D* Prendo importo da assicurare da TITA4 t.r.=J
080800001218     C                   movel     'J'           KTRC
080900001218     C     KTA4          chain     tita430c
081000001218     C                   if        %found(tita430c)
081100001218     C                   movel     ta4NOT        DSBL4J
081200010509     C* => istruzioni x VAB
081300001221     C                   z-add     §B4IAS        vabIAS
081400001221     C                   movel     §B4VAS        vabVAS
081500001221     C                   movel     §B4XCO        vabXCO
081600001227     C                   movel     §B4BAU        vabSCL
081700010131     C                   if        §B4BAI = 'S'
081800010131     C                   movel     §B4BAI        vabFTM
081900010131     C                   endif
082000010509     C* => istruzioni x VAC
082100010509     C                   z-add     §B4IAS        vacIAS
082200010509     C                   movel     §B4VAS        vacVAS
082300001218     C                   endif
082400001218     C*
082500001218     C                   ENDSR
082600001218     C*------------------------------------------------------------------------*
082700001218
082800001218
082900001218
083000001218
083100001218     C*------------------------------------------------------------------------*
083200001218     C* REPRMA - Routine di reperimento dati relativi al riferimento mittente alfabetico
083300001218     C*------------------------------------------------------------------------*
083400001218     C     REPRMA        BEGSR
083500001218     C*
083600060215     D* Prendo importo da assicurare da TITA4 t.r.=A
083700001218     C                   movel     'A'           KTRC
083800001218     C     KTA4          chain     tita430c
083900001218     C                   if        %found(tita430c)
084000010509     C* => istruzioni x VAB
084100001221     C                   eval      vabRMA = %subst(ta4NOT:1:15)
084200010509     C* => istruzioni x VAC
084300010509     C                   eval      vacRMA = %subst(ta4NOT:1:15)
084400001218     C                   endif
084500001218     C*
084600001218     C                   ENDSR
084700001218     C*------------------------------------------------------------------------*
084800001218
084900001218
085000001218
085100001218
085200001218     C*------------------------------------------------------------------------*
085300001218     C* REPDEST - Routine di reperimento dati relativi al destinatario della bolla (TITAA)
085400001218     C*           ... e ai dati del mittente originale
085500001218     C*------------------------------------------------------------------------*
085600001218     C     REPDEST       BEGSR
085700001218     C*
085800001218     D* Prendo il tipo anagrafica = D
085900001218     C                   movel     'D'           KTRC
086000010131     C     KTAA          chain     titaa30c
086100001218     C                   if        %found(titaa30c)
086200001221     C                   movel     taaRSC        vabRD2
086300001218     C                   endif
086400001218     C*
086500010509     C* Prendo il tipo anagrafica = O
086600001218     C                   movel     'O'           KTRC
086700001218     C     KTAA          chain     titaa30c
086800001218     C                   if        %found(titaa30c)
086900010509     C* => istruzioni x VAB
087000001221     C                   movel     taaRSC        vabRMO
087100001221     C                   movel     taaCAP        vabCMO
087200001221     C                   movel     taaNAZ        vabNMO
087300010509     C* => istruzioni x VAC
087400010509     C                   movel     taaRSC        vacRMO
087500001218     C                   endif
087600010509     C*
087700001222     C                   if        vabNMO = *blanks
087800001222     C                   move      '.'           vabNMO
087900001222     C                   endif
088000001218     C*
088100001218     C                   ENDSR
088200001218     C*------------------------------------------------------------------------*
088300001218
088400001218
088500001218
088600001218
088700001218     C*------------------------------------------------------------------------*
088800001218     C* REPCSB - Routine di reperimento dati relativi al contrassegno
088900001218     C*------------------------------------------------------------------------*
089000001218     C     REPCSB        BEGSR
089100001218     C*
089200001219     C     KCSB          chain     tncsb03l
089300001219     C                   if        %found(tncsb03l)
089400001218     C                   if        csbSTA = 9 and tasTSP <> 'P'
089500001218     C                   else
089600010509     C* => istruzioni x VAB
089700001221     C                   movel     csbTPA        vabTIC
089800001221     C                   move      csbTPI        vabTIC
089900001221     C                   z-add     csbCAS        vabCAS
090000001221     C                   movel     csbVCA        vabVCA
090100001221     C                   movel     csbGCA        vabGCA
090200010509     C* => istruzioni x VAC
090300010509     C                   movel     csbTPA        vactic
090400010509     C                   move      csbTPI        vactic
090500010509     C                   z-add     csbCAS        vacCAS
090600010509     C                   movel     csbVCA        vacVCA
090700010626     C                   z-add     csbNDP        vacNDP
090800010626     C                   z-add     csbDDP        vacDDP
090900001218     C                   endif
091000021023     C                   if        csbDDP > *zeros
091100021023     C                   eval      VACXX0 = '1'
091200021023     C                   endif
091300021023     C                   if        csbSTA = 9
091400021023     C                   eval      VACXX0 = '2'
091500021023     C                   endif
091600001218     C                   endif
091700001218     C*
091800001218     C                   ENDSR
091900001218     C*------------------------------------------------------------------------*
092000001218
092100010509
092200010509
092300010509
092400010626     C*------------------------------------------------------------------------*
092500010626     C* REPLA - Routine di reperimento dati relativi alla Lettera di Anomalia
092600010626     C*------------------------------------------------------------------------*
092700010626     C     REPLA         BEGSR
092800010626     C*
092900010626     C     KDCT          CHAIN     FNDCT02L
093000010626     C                   IF        %found(FNDCT02L)
093100010626     C                   Z-ADD     dctAAC        vacAAC
093200010626     C                   Z-ADD     dctFIL        vacPOC
093300010626     C                   Z-ADD     dctNCA        vacNCA
093400010626     C                   ENDIF
093500010626     C*
093600010626     C                   ENDSR
093700010626     C*------------------------------------------------------------------------*
093800010626
093900010626
094000010626
094100010626
094200010509     C*------------------------------------------------------------------------*
094300010509     C* CARTBL - Routine di caricamento dati tabellati
094400010509     C*------------------------------------------------------------------------*
094500010509     C     CARTBL        BEGSR
094600010509     C*
094700010509     C                   Z-ADD     0             W                 4 0
094800010509     C                   MOVEA     *HIVAL        T2A
094900010509     C                   MOVEA     *HIVAL        C2A
095000010509     C                   MOVEL     '2A'          COD
095100010509     C     KTAB          CHAIN     TABEL                              31
095200010509     C     *IN31         DOWEQ     '0'
095300010509     C     TBLFLG        IFEQ      ' '
095400010509     C                   MOVEL     TBLUNI        DS2A
095500010509     C     §2AFTC        IFNE      *BLANKS
095600010509     C                   ADD       1             W
095700010509     C                   MOVEL     §2AFTC        T2A(W)
095800010509     C                   MOVEL     TBLKEY        C2A(W)
095900010509     C                   ENDIF
096000010509     C                   ENDIF
096100010509     C     KTAB          READE     TABEL                                  31
096200010509     C                   ENDDO
096300010509     C*
096400010509     C                   ENDSR
096500010509     C*---------------------------------------------------------------*
096600010509
096700021028
096800010509
096900021028     C*-------------------------------------------------------------------*
097000021028     C* CONVERSIONE IN MONETA ED EVENTUALE ARROTONDAMENTO
097100021028     C*-------------------------------------------------------------------*
097200021028     C*
097300021028     C     CNVEUR        BEGSR
097400021028     C*
097500021028    4C     COMIMP        IFNE      *ZEROS
097600021028     C     COMDIV        ANDNE     'EUR'
097700021028     C                   CLEAR                   YEUR
097800021028     C                   MOVEL     COMDIV        YECDVI
097900021028     C                   MOVEL     'EUR'         YECDVO
098000021028     C                   Z-ADD     COMIMP        YECIMI
098100021028     C                   MOVE      '3'           YECDCO
098200021028     C*
098300021028     C                   CALL      'YEURCO'
098400021028     C                   PARM                    YEUR
098500021028     C*
098600021028    5C     YECESI        IFEQ      ' '
098700021028     C                   Z-ADD     YECIMO        COMIMP           30 9
098800021028     C                   ELSE
098900021028     C                   Z-ADD     *ZEROS        COMIMP
099000021028    5C                   ENDIF
099100021028     C*
099200021028    5C                   ENDIF
099300021028     C*
099400021028     C* MANTENGO SEMPRE INIZIALIZZATA LA DIVISA DI INPUT
099500021028     C                   MOVEL     *BLANKS       COMDIV            3
099600021028     C*
099700021028     C                   ENDSR
099800021028     C*------------------------------------------------------------------------*
099900040625
100000040625
100100040625
100200040625     C*----------------------------------------------------*
100300040625     C*  CONTROLLO NUMERICITA' CAMPI
100400040625     C*----------------------------------------------------*
100500040625     C     CHKNUM        BEGSR
100600040625     C*
100700040625     C                   Z-ADD     1             jStrNum           3 0
100800040625     C                   MOVEL     '1'           wStrNumFlg        1
100900040625     C*
101000040625     C                   DOW       jStrNum <= %len(%trim(wStrNum))
101100040625     C                   IF        %scan(%subst(%trim(wStrNum):jStrNum:1):
101200040625     C                             ConstNUM) = 0
101300040625     C                   MOVEL     '0'           wStrNumFlg
101400040625     C                   LEAVE
101500040625     C                   ENDIF
101600040625     C                   ADD       1             jStrNum
101700040625     C                   ENDDO
101800040625     C*
101900040625     C                   ENDSR
102000040625     C*------------------------------------------------------------------------*
102100021028
102200021028
102300001218
102400001218     C*------------------------------------------------------------------------*
102500001218     C* *INZSR - ROUTINE INIZIALE
102600001218     C*------------------------------------------------------------------------*
102700001218     C     *INZSR        BEGSR
102800001218     C*
102900010726     C* Ricevo in input il parametro indicante l'ID LANCIO
103000010726     C     *ENTRY        PLIST
103100010726     C                   PARM                    IDL               5
103200010927     C                   PARM                    TPL               1
103300021004     C                   PARM                    TIN               1
103400030304     C                   PARM                    DAT               8
103500040206     C                   PARM                    DOPPI             1
103600050309     C                   PARM                    NOTAS             1
103700030304     C*
103800030304     C                   MOVE      DAT           numDAT            8 0
103900010726     C*
104000001218     C                   Z-ADD     1             CODUT
104100001218     C*
104200001218     C* Definizioni chiavi
104300010509     C     K34C          KLIST                                                  TITAS34C - Parziale
104400010509     C                   KFLD                    fls
104500010509     C                   KFLD                    nrs
104600010509     C                   KFLD                    ncd
104700010131     C*
104800010509     C     KTA4          KLIST                                                  TITA430C - Completa
104900001218     C                   KFLD                    tasAAS
105000001218     C                   KFLD                    tasLNP
105100001218     C                   KFLD                    tasNRS
105200001218     C                   KFLD                    tasNSP
105300001218     C                   KFLD                    KTRC              1
105400001218     C*
105500010509     C     KTAA          KLIST                                                  TITAA30C - Completa
105600001218     C                   KFLD                    tasAAS
105700001218     C                   KFLD                    tasLNP
105800001218     C                   KFLD                    tasNRS
105900001218     C                   KFLD                    tasNSP
106000001218     C                   KFLD                    KTRC
106100001218     C*
106200010509     C     KCSB          KLIST                                                  TNCSB03L - Completa
106300001218     C                   KFLD                    tasAAS
106400001218     C                   KFLD                    tasLNP
106500001218     C                   KFLD                    tasNRS
106600001218     C                   KFLD                    tasNSP
106700010626     C*
106800010626     C     KDCT          KLIST                                                  FNDCT02L - Completa
106900010626     C                   KFLD                    tasAAS
107000010626     C                   KFLD                    tasLNP
107100010626     C                   KFLD                    tasNRS
107200010626     C                   KFLD                    tasNSP
107300010509     C*
107400010509     C     KEVB          KLIST                                                  FNEVB01L - Completa
107500010509     C                   KFLD                    tasAAS
107600010509     C                   KFLD                    tasLNP
107700010509     C                   KFLD                    tasNRS
107800010509     C                   KFLD                    tasNSP
107900010509     C*
108000010509     C     KTAB          KLIST                                                  * tabelle
108100010509     C                   KFLD                    CODUT             1 0
108200010509     C                   KFLD                    COD               2
108300010509     C*
108400010509     C     KTAB2         KLIST                                                  * tabelle
108500010509     C                   KFLD                    CODUT
108600010509     C                   KFLD                    COD
108700010626     C                   KFLD                    KEY               8
108800010626     C*
108900010726     C     KVAC          KLIST                                                  FNVACPT  - Completa
109000010626     C                   KFLD                    tasAAS
109100010626     C                   KFLD                    tasLNP
109200010626     C                   KFLD                    tasNRS
109300010626     C                   KFLD                    tasNSP
109400010726     C*
109500010726     C     KVACL         KLIST                                                  FNVACPTL - Completa
109600010726     C                   KFLD                    tasAAS
109700010726     C                   KFLD                    tasLNP
109800010726     C                   KFLD                    tasNRS
109900010726     C                   KFLD                    tasNSP
110000010726     C                   KFLD                    IDL
110100001218     C*
110200001218     C* Determino la data corrente
110300001218     C                   TIME                    W0140            14 0
110400001218     C                   MOVE      W0140         WDTGIO            8 0
110500001218     C                   Z-ADD     WDTGIO        G02DAT
110600001218     C                   MOVEL     *BLANK        G02ERR
110700001218     C                   CALL      'XSRDA8'
110800001218     C                   PARM                    WLBDAT
110900001218     C                   MOVEL     G02INV        DATEU             8 0
111000001218     C*
111100001218     C                   ENDSR
111200010509
111300010627     Oqsysprt   E            ncdlost           2
111400010509     O                       DSNCDPT
