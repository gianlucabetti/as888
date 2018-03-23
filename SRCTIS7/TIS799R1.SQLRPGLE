000100010411     |********************************************************************************************|
000200090918     |********************************************************************************************|
000300090918     h* Creazione file estratto bolle -TIVTA10-
000400090917     h DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*CALLER)
000500991026     h DECEDIT('0,') DATEDIT(*DMY.)
000600991102     f*--------------------------------------------------------------------------------------------*
000700991102     f* Data base
000800991102     f*--------------------------------------------------------------------------------------------*
000900991110     f*---
001000991110     f* Estratto bolle
001100991110     f*---
001200090918     ftivta10f  O  A E             DISK    INFDS(InfTIVTA)                      *Estratto bolle
001300091109     f                                     infsr(*pssr)
001400091229     f                                     block(*no)
001500090918     ftivta11l  UF   E           K DISK                                         *Estratto bolle
001600090918     f                                     RENAME(tivta000:tivta011)
001700091109     f                                     infsr(*pssr)
001800091116     ftivta12l  UF   E           K DISK                                         *Estratto bolle
001900091116     f                                     RENAME(tivta000:tivta012)
002000091109     f                                     infsr(*pssr)
002100091116     ftivta13l  UF   E           K DISK                                         *Estratto bolle
002200091116     f                                     RENAME(tivta000:tivta013)
002300091116     f                                     infsr(*pssr)
002400091214 xxx f***tivta10lf UF A E             DISK    PREFIX(L_)                           *Log crea TIVTA
002500991102     f*---
002600991110     f* Bolle di sede
002700991110     f*---
002800010409     f* ---------> vista logica unique
002900010409     ftitas30c  IF   E           K DISK                                         *Bolle
003000010409     f                                     IGNORE(titasp00)
003100010409     f                                     RENAME(titas000:titas030)
003200010409     f                                     RENAME(titas010:titas130)
003300991110     f* ---------> ordinamento per cliente di fatturazione
003400991110     ftitas31c  IF   E           K DISK                                         *Bolle
003500991118     f                                     IGNORE(titasp00)
003600991110     f                                     RENAME(titas000:titas031)
003700991110     f                                     RENAME(titas010:titas131)
003800991110     f* ---------> ordinamento per cliente mittente
003900991105     ftitas33c  IF   E           K DISK                                         *Bolle
004000991118     f                                     IGNORE(titasp00)
004100991110     f                                     RENAME(titas000:titas033)
004200991110     f                                     RENAME(titas010:titas133)
004300991102     f*---
004400991102     f* Bolle di sede - Riferimenti
004500991102     f*---
004600991110     ftita430c  IF   E           K DISK                                         *Riferimenti bolle
004700001002     f*---
004800001002     f* Bolle di sede - Estensioni anagrafiche clienti non codificati
004900001002     f*---
005000001002     ftitaa30c  IF   E           K DISK                                         *Estensione anag.
005100161117     f*---
005200161117     f* Bolle di filiale (in partenza)
005300161117     f*---
005400161117     ffnblp01l  IF   E           K DISK
005500001002     f*---
005600001002     f* Anagrafiche contabili - clienti codificati
005700001002     f*---
005800020516     fcnaco00f  IF   E           K DISK                                         *Clienti codificati
005900991110     f*---
006000991117     f* Tabelle
006100991110     f*---
006200010409     Ftntbe01l  IF   E           K DISK                                         *Tabelle
006300991102     d*--------------------------------------------------------------------------------------------*
006400991102     d* Data structure
006500991102     d*--------------------------------------------------------------------------------------------*
006600991102     d*---
006700991102     d* Ds
006800090918     d*---
006900010409     d* reperimento clienti da cliente unificante
007000090918     d*---
007100991026     d tibs10ds      E DS
007200991026     d  sksc11                21   5520  0 DIM(500)                             *schiera clienti
007300090918     d*---
007400010409     d* DS riferimenti
007500090918     d*---
007600991102     d dta4a         E DS
007700070927     d dsbl4E        E DS
007800090918     d*---
007900010409     d* DS composizione data spedizione
008000090918     d*---
008100991026     d                 DS                  INZ
008200991026     d  tasaas                 1      4  0                                       -annp
008300991026     d  tasmgs                 5      8  0                                       -mese/giorno
008400991026     d tasdsp                  1      8  0                                      *data spedizione
008500090918     d*---
008600090918     d* DS Input di procedura
008700090918     d*---
008800991109     d tis799dsI     E DS
008900991126     d*---
009000991126     d* Variabili temporali
009100991126     d*---
009200991203     d                 DS
009300991126     d wdata                           D   DATFMT(*ISO) INZ(D'1999-01-01')
009400991102     d*---
009500991102     d* Variabili non riferite al data base
009600991102     d*---
009700991102     d n14             S             14  0                                      *Numerico 14
009800991102     d n8              S              8  0                                      *Numerico 8
009900010709     d n4              S              4  0                                      *Numerico 4
010000991102     d datcor          S              8  0                                      *data corrente a/m/g
010100030129     d datcorpre       S              8  0                                      *data limite cli BAR
010200991110     d wi              S              5  0                                      *indice di lavoro
010300991110     d wfkc            S              1                                         *tipo cliente (K/C)
010400010409     d flgbol          S              1                                         *flag x tipo elab.
010500091207     d flgcli          S              1                                         *flag x tipo elab.
010600010409     d depcli          S              8  0                                      *cod cli. x "cli"
010700991109     d*---
010800991109     d* Variabili di controllo
010900991109     d*---
011000991110     d $ftas           S              1    INZ('N')                             *fine lettura TITAS
011100991109     d $recok          S              1    INZ('N')                             *validità record
011200040514      *-------------------
011300040514      * DS lancio scrittura/cancellazione elenco dei colli in TIVTB00F
011400040514      *-------------------
011500040517     D TIS7F1DSI     E DS                  INZ
011600040514     D  OpeIF1       E
011700040517     D  LibIF1       E
011800040517     D  CmtCtlIF1    E                     INZ(*OFF)
011900040517     D  CommitIF1    E                     INZ(*OFF)
012000040514     D TIS7F1DSO     E DS
012100040514     D                                     INZ
012200040514     D InfTIVTA        DS
012300090918     D  LibTIVTA              93    102
012400090918     D*
012500090918     D tivssds       E DS                  EXTNAME(tivss00f) QUALIFIED
012600090918     D dtle          E DS
012700090918     D sktle           S              2    DIM(20)
012800090918     D sksc8           S              8  0 DIM(500)                             *clienti
012900090918     D jtle            S              5  0                                      *indici
013000090918     D Tascli          S                   LIKE(tasksc)                         *lett. titas31/33c
013100090918     D ktadsp          S                   LIKE(vtadsp)                         *lettura tivta10f
013200091207 xxx D*** RWRINEW         S              6  0
013300091207 xxx D*** RDELNEW         S              6  0
013400090918     D*
013500090918     D/COPY GAITRASRC/SRCPROTOPR,TIS7701R
013600091124     D/COPY GAITRASRC/SRCPROTOPI,TIS7701R
013700090918     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
013800090918     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
013900090921     D*
014000090921     D pgmstatus      SDS
014100090921     D  JOB_NAME             244    253                                         * Job name
014200090921     D  USER                 254    263                                         * User name
014300090921     D  JOB_NUM              264    269                                         * Job number
014400090918     c*--------------------------------------------------------------------------------------------*
014500090918     c* Main lines
014600991102     c*--------------------------------------------------------------------------------------------*
014700091207     C*
014800091207     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
014900091207     C
015000091207     C/EXEC SQL
015100091207     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
015200091207     C/END-EXEC
015300091207     C*
015400091207
015500991110     c*
015600991110     c* cancella l'estratto bolle del CLIENTE UNIFICANTE
015700010409     c                   EXSR      delvta
015800010409     c*
015900010409     c* a seconda dei parametri ricevuti in ingresso => eseguo differenti elaborazioni
016000010409     c                   IF        flgbol = '0'
016100010409     c*
016200010409     c* imposta i clienti da elaborare
016300010409     c                   EXSR      elacli
016400991109     c*
016500991109     c* elabora tutti i cienti memorizzati
016600090917     c     1             DO        500           wi
016700090917     c                   IF        sksc8(wi) > *zeros
016800991110     c*
016900991110     c* elabora le bolle per CLIENTE di FATTURAZIONE
017000090918     c                   EVAL      wfkc = 'K'                                   cliente fatturazione
017100090918     c                   EXSR      elatas                                       *elabora bolle
017200991110     c*
017300991110     c* elabora le bolle per CLIENTE MITTENTE
017400090918     c                   EVAL      wfkc = 'C'                                   cliente mittente
017500090918     c                   EXSR      elatas                                       *elabora bolle
017600090918     c*
017700090918     c                   ELSE
017800090918     c                   LEAVE
017900090917     c                   ENDIF
018000090917     c                   ENDDO                                                  *fine clienti
018100010409     c*
018200010409     c                   ELSE
018300010409     c*
018400010409     c* effettua considerazioni/elaborazioni x singola bolla
018500090918     c                   EXSR      elabol
018600010409     c*
018700010409     c                   ENDIF
018800010409     c*
018900091214 xxx c***                eval      l_SUNI99 = SUNI99
019000091214 xxx c***                eval      l_KSUI99 = KSUI99
019100091214 xxx c***                eval      l_CLII99 = CLII99
019200091214 xxx c***                eval      l_AASI99 = AASI99
019300091214 xxx c***                eval      l_MGSI99 = MGSI99
019400091214 xxx c***                eval      l_LNPI99 = LNPI99
019500091214 xxx c***                eval      l_NRSI99 = NRSI99
019600091214 xxx c***                eval      l_NSPI99 = NSPI99
019700091214 xxx c***                eval      l_TBLI99 = TBLI99
019800091214 xxx c***                eval      l_OPEI99 = OPEI99
019900091214 xxx c***                eval      l_LOGDATA = datcor
020000091214 xxx c***                write(e)  tivtalog
020100091207     c*
020200991110     c                   SETON                                            lr
020300991110     c*--------------------------------------------------------------------------------------------*
020400090918 ttt c* elabora le bolle per CLIENTE di FATTURAZIONE oppure CLIENTE MITTENTE
020500991110     c*--------------------------------------------------------------------------------------------*
020600991110     c     elatas        BEGSR
020700991110     c*
020800991110     c* posizionamento e prima lettura -bolle-
020900991110     c                   EXSR      settas
021000991110     c*
021100991110     c* ciclo fino a fine file
021200090917     c                   DOW       $ftas='N'
021300991110     c*
021400991110     c* memorizza i dati nel file -estratto bolle-
021500090918     c                   EXSR      scrvta
021600991110     c*
021700991110     c* lettura successiva -bolle-
021800991110     c                   EXSR      lettas
021900090917     c                   ENDDO
022000991110     c*
022100991110     c                   ENDSR
022200991110     c*--------------------------------------------------------------------------------------------*
022300090917 ttt c* posizionamento e prima lettura -bolle-
022400991110     c*--------------------------------------------------------------------------------------------*
022500991110     c     settas        BEGSR
022600991110     c*
022700991110     c* reimposta variabili di lavoro
022800991110     c                   EVAL      $ftas = 'N'
022900991110     c*
023000991110     c* si posiziona x CLIENTE di FATTURAZIONE (1° giro) oppure x CLIENTE MITTENTE (2° giro)
023100090917     c                   MOVE      sksc8(wi)     Tascli
023200090918     c                   IF        wfkc = 'K'                                   *cliente fattur.ne
023300090917     c     Tascli        SETLL     titas31c
023400090917     c                   ELSE                                                   *cliente mittente
023500090917     c     Tascli        SETLL     titas33c
023600090917     c                   ENDIF
023700090917     c                   IF        %equal                                       *no fine file
023800090917     c                   EXSR      lettas                                       *lettura record
023900090917     c                   ELSE                                                   *fine file
024000090917     c                   MOVEL     'S'           $ftas                          *fine bolle
024100090917     c                   ENDIF
024200991110     c*
024300991110     c                   ENDSR
024400991110     c*--------------------------------------------------------------------------------------------*
024500090918 ttt c* lettura successiva -bolle-
024600991110     c*--------------------------------------------------------------------------------------------*
024700090918     c     lettas        BEGSR
024800090917     c*
024900991110     c* legge x CLIENTE di FATTURAZIONE (1° giro) oppure x CLIENTE MITTENTE (2° giro)
025000991110     c                   MOVEL     'N'           $recok
025100090917     c     $ftas         DOWEQ     'N'
025200161116     c     $recok        ANDEQ     'N'
025300161116     c                   setoff                                       70
025400161116     c                   setoff                                       71
025500090917     c                   IF        wfkc = 'K'                                   *cliente fattur.ne
025600091117     c     Tascli        READE     titas31c
025700091117     c                   Z-ADD     tasksc        depcli
025800090917     c                   ELSE                                                   *cliente mittente
025900091117     c     Tascli        READE     titas33c
026000091117     c                   Z-ADD     tasccm        depcli
026100090917     c                   ENDIF
026200090923     c                   IF        %eof                                         *fine file
026300090917     c                   MOVEL     'S'           $ftas                          *fine bolle
026400090917     c                   ELSE
026500090917     c                   EXSR      chktas                                       *controlla record
026600090917     c                   ENDIF
026700090917     c                   ENDDO
026800991110     c*
026900991110     c                   ENDSR
027000991110     c*--------------------------------------------------------------------------------------------*
027100090918 ttt c* controlla record -bolle-
027200090918     c*--------------------------------------------------------------------------------------------*
027300090918     c     chktas        BEGSR
027400991110     c*
027500991110     c                   MOVEL     'S'           $recok                         *record valido
027600991130     C*
027700000105     c* data spedizione compresa fra data corrente e data corrente - 2 mesi, altrimenti esclude
027800090917     c                   IF        $recok = 'S'                                 *record valido
027900090917     c                   IF        tasdsp >= datcorpre  AND
028000090917     c                             tasdsp <= datcor
028100090917     c                   ELSE
028200090917     c                   MOVEL     'N'           $recok                         *record NON valido
028300090917     c                   ENDIF
028400090917     c                   ENDIF
028500090917     c*
028600090917     c* se lettura x CLIENTE FATTURAZ. esclude le bolle con cliente mittente = cliente fatturazione
028700090917     c                   IF        $recok = 'S'                                 *record valido
028800090918     c                   IF        wfkc = 'K'
028900161116     c                   IF        tasksc = tasccm
029000090917     c                   MOVEL     'N'           $recok                         *record NON valido
029100090917     c                   ENDIF
029200090917     c                   ENDIF
029300090917     c                   ENDIF
029400090917     c
029500090917     c* se bolla di recupero, esclude
029600090917     c                   IF        $recok = 'S'                                 *record valido
029700090917     c                   IF        tastbl = 'AR' OR                             *NO bolla RECUPERO
029800000124     c                             tastbl = 'A3' OR
029900000124     c                             tastbl = 'F3' OR
030000000124     c                             tastbl = 'AP' OR
030100000124     c                             tastbl = 'FC'
030200000120     c                   MOVEL     'N'           $recok                         *record NON valido
030300090917     c                   ENDIF
030400090917     c                   ENDIF
030500090917     c
030600090917     c*
030700000105     c* se bolla figlia, esclude
030800090917     c                   IF        $recok = 'S'                                 *record valido
030900090917     c                   MOVEL     *blanks       wEsito1           1
031000090917     c                   EVAL      wEsito1 = %char(UBLBLSPE_GetLblTyp(
031100090917     c                                                tasaas
031200090917     c                                               :taslnp
031300090917     c                                               :tasnrs
031400090917     c                                               :tasnsp
031500090917     c                                               :pOutLblTyp
031600090917     c                                               :pOutAnnoBO
031700090917     c                                               :pOutLineaParBO
031800090917     c                                               :pOutSerieBO
031900090917     c                                               :pOutNumSpedBO
032000090917     c                                               :pOutDcmBO
032100090917     c                                               :pOutCcaBO
032200090917     c                                               :pOutRblBO))
032300090917     c                   IF        pOutLblTyp = 'O'
032400090917     c                   ELSE
032500000105     c                   MOVEL     'N'           $recok                         *record NON valido
032600090917     c                   ENDIF
032700090917     c                   ENDIF
032800991110     c*
032900991110     c                   ENDSR
033000090917     c*--------------------------------------------------------------------------------------------*
033100090918 ttt c* memorizza i dati nel file -estratto bolle-
033200991110     c*--------------------------------------------------------------------------------------------*
033300090918     c     scrvta        BEGSR
033400040517     c*
033500091022     c                   EXSR      impkeyvta                                    *imposta la key
033600091014     c*
033700091014     c* verifico se chiave univoca già presente..
033800091022     c     Keyvta_12     setll     tivta12l
033900091014     c                   if        %equal(tivta12l)
034000091014     c                   else
034100091022     c                   EXSR      impvta                                       *imposta i campi
034200090917     c                   WRITE(e)  tivta000                                     *scrive record
034300091113     c*
034400091127 xxx c***                if        not %error
034500091127 xxx c***                add       1             RWRINEW
034600091127 xxx c***                endif
034700091113     c*
034800040517     c* scrive bolla anche nel file estensione
034900090917     C                   CLEAR                   TIS7F1DSI
035000040517     c                   EVAL      OpeIF1 = 'W'
035100040517     c                   EXSR      scrvtb
035200991110     c*
035300091022     c                   endif
035400091022     c*
035500090918     c                   ENDSR
035600091022     c*--------------------------------------------------------------------------------------------*
035700091022 ttt c* imposta campi chiave di TIVTA per la scrittura
035800091022     c*--------------------------------------------------------------------------------------------*
035900091022     c     impkeyvta     BEGSR
036000091022     c*
036100091022     c* imposta chiave
036200091022     c                   EVAL      vtaksu = tivssds.vssksu                      *cliente unificante
036300091022     c                   EVAL      vtadsp = tasdsp                              *data spedizione
036400091022     c                   EVAL      vtalnp = taslnp                              *linea partenza
036500091022     c                   EVAL      vtanrs = tasnrs                              *serie
036600091022     c                   EVAL      vtansp = tasnsp                              *spedizione
036700091022     c                   EVAL      vtatbl = tastbl                              *tipo bolla
036800091022     c*
036900091022     c                   ENDSR
037000991026     c*--------------------------------------------------------------------------------------------*
037100090918 ttt c* imposta campi di TIVTA per la scrittura
037200991026     c*--------------------------------------------------------------------------------------------*
037300090918     c     impvta        BEGSR
037400091022     c*
037500090918     c*
037600090918     c* <--- inserimento logica x reperimento SUBK ----->
037700091014     c*
037800091014 xxx c*                  EVAL      vtasubk = %trim(xxx)
037900091014     c*
038000090918     c* <----------------------------------------------->
038100090918     c*
038200091117     c                   MOVEL     depcli        vtacli                         *cliente KSC/CCM
038300090929     c                   EVAL      vtafkc = wfkc                                *tipo cliente
038400991110     c*
038500001002     c* imposta campi da TITAS
038600991110     c                   EVAL      vtarmn = tasrmn                              *riferim mittente
038700091014     c                   EVAL      vtarsd = %trim(tasrsd)                       *destinatario
038800001002     c                   EVAL      vtapoa = taslna                              *punto op. di arrivo
038900001002     c                   EVAL      vtaprd = tasprd                              *provincia di dest.
039000001002     c                   IF        tasfgc = 'S'
039100001002     c                   EVAL      vtagia = '1'                                 *stato giacenza "SI"
039200001002     c                   ELSE
039300001002     c                   EVAL      vtagia = '2'                                 *stato giacenza "?"
039400001002     c                   ENDIF
039500001002     c*
039600091014     c                   EVAL      vtalna = %trim(taslod) +' ' + tasnzd         *destinazione
039700001002     c*
039800090917     c                   IF        tasdcm > *zeros                              *consegnata
039900090917     c                   EVAL      vtasta = '1'                                 *stato consegnata
040000090917     c                   ELSE                                                   *non consegnata
040100090917     c                   EVAL      vtasta = '2'                                 *stato ocnsega
040200090917     c                   ENDIF
040300090917     c*
040400090917     c* imposta la ragione sociale del cliente mittente
040500161116     c  N71              clear                   vtarsm
040600090917     c                   MOVE      tasccm        n4
040700090917     c                   IF        n4 <> 8888                                   *cl NON codificato
040800090918     c                   movel     1             acokut
040900090918     c                   z-add     151           acokcc
041000090918     c                   z-add     tasccm        acoksc
041100090918     c     Keyaco        CHAIN     cnaco00f
041200090918     c                   IF        %found(cnaco00f)
041300091014     c                   eval      vtarsm = %trim(acorag)
041400090917     c                   ENDIF
041500090917     c                   ELSE                                                   *cl codificato
041600090918     c                   MOVEL     'M'           ta4trc
041700090918     c     Keyta4        CHAIN     titaA30c
041800090918     c                   IF        %found(titaA30c)
041900091014     c                   eval      vtarsm = %trim(taarsc)
042000090917     c                   ENDIF
042100090917     c                   ENDIF
042200090917     c*
042300991110     c* imposta campi da TITA4
042400991102     c                   CLEAR                   dta4a
042500090918     c                   MOVEL     'A'           ta4trc
042600090918     c     Keyta4        CHAIN     tita430c
042700090918     c                   IF        %found(tita430c)
042800991102     c                   MOVEL     ta4not        dta4a
042900091014     c                   EVAL      vtarma = %trim(§ta4arma)                     *riferim alfabetico
043000011105     c****
043100050124     c* Personalizzazione HERBALIFE --> prende 8 char del riferimento alfabetico e li
043200011105     c*                                 mette nel campo VTAA08 (per questo cliente è il N° ORDINE)
043300011105     c****
043400110721     c                   IF        vtaksu = '00890896' or
043500110721     c                             vtaksu = '01660912'
043600091014     c                   EVAL      vtaa08 = %TRIM(%SUBST(§ta4arma:3:8))
043700090917     c                   ENDIF
043800090917     c*
043900090917     c                   ELSE                                                   *non trovato
044000090917     c*
044100161116     c  N71              CLEAR                   vtarma
044200161116     c  N71              CLEAR                   vtaa08
044300090917     c                   ENDIF
044400070927     c
044500070927     c* Riferimento partner estero
044600070927     c                   CLEAR                   dsbl4e
044700090918     c                   MOVEL     'E'           ta4trc
044800090918     c     Keyta4        CHAIN     tita430c
044900090918     c                   IF        %found(tita430c)
045000070927     c                   MOVEL     ta4not        dsbl4e
045100091014     c                   eval      vtarpt = %trim(§b4erp)
045200070927     c                   else
045300091014     c                   clear                   vtarpt
045400070927     c                   endif
045500991026     c*
045600991026     c                   ENDSR
045700010409     c*--------------------------------------------------------------------------------------------*
045800161116 ttt c* verifica se la bolla è da elaborare
045900161116     c*--------------------------------------------------------------------------------------------*
046000161116     c     elabol        BEGSR
046100161116     c*
046200161116     c                   setoff                                       70
046300161116     c                   setoff                                       71
046400010409     c*
046500090918     c* per prima cosa "chaino" la bolla corrente
046600090918     c     Keytas_30     chain     titas30c
046700010529     c                   if        %found(titas30c)
046800161116     c                   seton                                        70
046900161116     c                   else
047000161116     c*
047100161116     c* "chaino" in partenza la bolla corrente
047200161116     c     Keyblp_01     chain     fnblp01l
047300161116     c                   if        %found(fnblp01l)
047400161116     c                   seton                                        70
047500161116     c                   seton                                        71
047600161116     c*
047700161116     c* "overrido" i campi TAS con quelli BLP
047800161116     c                   eval      tasAAS = blpAAS
047900161116     c                   eval      tasLNP = blpLNP
048000161116     c                   eval      tasNRS = blpNRS
048100161116     c                   eval      tasNSP = blpNSP
048200161116     c                   eval      tasLNA = blpLNA
048300161116     c                   eval      tasMGS = blpMGS
048400161118     c                   eval      tasKSC = blpKSC
048500161118     c                   if        blpCCM > 0
048600161116     c                   eval      tasCCM = blpCCM
048700161118     c                   else
048800161118     c                   eval      tasCCM = blpKSC
048900161118     c                   endif
049000161116     c                   eval      tasRSD = blpRSD
049100161116     c                   eval      tasLOD = blpLOD
049200161116     c                   eval      tasPRD = blpPRD
049300161116     c                   eval      tasNZD = blpNZD
049400161116     c                   eval      tasDCM = blpDCM
049500161116     c                   eval      tasRMN = blpRMN
049600161116     c                   eval      tasTBL = *blanks
049700161116     c                   eval      tasFGC = *blanks
049800161116     c                   eval      vtaRSM = blpRSM
049900161116     c                   eval      vtaRMA = blpRMA
050000161118     c                   eval      vtaa08 = %trim(%subst(blpRMA:3:8))
050100161116     c                   endif
050200161116     c                   endif
050300090918     c*
050400161116     c                   if        *in70
050500090918     c                   eval      wfkc = *blanks                               *elab. x bolla
050600090918     c*
050700090918     c* verifica se è una bolla ba considerare
050800090918     c                   exsr      chktas
050900090918     c                   if        $recok = 'S'
051000090918     c*
051100090918     c* se il cliente di fatturazione ed il cliente mittente sulla bolla sono diversi...
051200090918     c* tento di inserire in TIVTA x CCM
051300090918     c                   if        tasccm <> tasksc
051400090918     c                   z-add     tasccm        depcli
051500091116     c                   exsr      rtvStAlone
051600091116     c                   exsr      rtvFamily
051700090918     c                   endif
051800090918     c*
051900090918     c* x KSC tento d inserire in TIVTA sempre e cmq
052000090918     c                   z-add     tasksc        depcli
052100091116     c                   exsr      rtvStAlone
052200090918     c                   exsr      rtvFamily
052300090918     c*
052400090918     c                   endif
052500090918     c                   endif
052600010424     c*
052700010409     c                   ENDSR
052800090918     c*--------------------------------------------------------------------------------------------*
052900090918 ttt c* reperisce la "familia" x tutti i tipi legami d tipo "internet"
053000090918     c*--------------------------------------------------------------------------------------------*
053100090918     c     rtvFamily     BEGSR
053200090918     c*
053300090918     c* ciclo x tutti i legami d tipo "internet"
053400090918     c                   z-add     1             jtle
053500090918     c                   dow       jtle <= %elem(sktle)
053600090918     c                   if        sktle(jtle) <> *blanks
053700090918     c*
053800090918     c                   clear                   sksc8                          *clienti da elab.
053900091106     c* cerco il padre
054000090918     c                   CLEAR                   tibs10ds
054100090918     c                   EVAL      d10drf = datcor                              *data riferimento
054200091020     c                   EVAL      d10tle = sktle(jtle)                         *legame
054300090918     c                   EVAL      d10paf = 'P'                                 *cerca il padre
054400090918     c                   Z-ADD     depcli        d10cod
054500090918     c                   CALL      'TIBS10R'                                    *cerca figli
054600090918     c                   PARM                    tibs10ds
054700091123     c                   IF        d10err = ' '                                 *NO errore: si figli
054800090918     c                   Z-ADD     d10cop        sksc8(1)                       *cliente padre
054900090918     c*
055000090918     c* controlla se il padre reperito è abilitato al TRACK & TRACING alla data corrente
055100090918     c                   MOVE      sksc8(1)      rqsKsu
055200090918     c                   CLEAR                   tivssds
055300090918     c
055400090918     c                   CALLP     Selettore_record_TIVSS('GETRCDVSS'
055500091022     c                             : '1' : rqsKsu : 'TT' : *blanks : 0
055600090918     c                             : tivssds : rpyEsito
055700090918     c                             )
055800090918     c*
055900091116     c* se padre abilitato => inserisco il record nel TIVTA
056000091117     c                   if        rpyEsito = *zeros         AND
056100091117     c                             tivssds.vssksu <> *blanks AND
056200091117     c                             tivssds.vssksu <> *zeros
056300091117     c                   if        tivssds.vsstle = sktle(jtle)
056400090918     c                   exsr      scrvta
056500091109     c                   endif
056600091109     c                   else
056700091127 xxx c***                add       99505         RWRINEW
056800091109     c                   endif
056900091123     c*
057000091123     c                   ENDIF
057100090918     c*
057200090918     c                   else
057300090918     c                   leave
057400090918     c                   endif
057500090918     c*
057600090918     c                   add       1             jtle
057700090918     c                   enddo
057800090918     c*
057900090918     c                   ENDSR
058000091116     c*--------------------------------------------------------------------------------------------*
058100091116 ttt c* tenta d elaborare anche x codice cliente "stand alone"
058200091116     c*--------------------------------------------------------------------------------------------*
058300091116     c     rtvStAlone    BEGSR
058400091116     c*
058500091116     c                   clear                   sksc8                          *clienti da elab.
058600091116     c                   Z-ADD     depcli        sksc8(1)                       *cliente corrente
058700091116     c*
058800091116     c* controlla se il cliente "stand-alone" è abilitato al TRACK & TRACING alla data corrente
058900091116     c                   MOVE      sksc8(1)      rqsKsu
059000091116     c                   CLEAR                   tivssds
059100091116     c
059200091116     c                   CALLP     Selettore_record_TIVSS('GETRCDVSS'
059300091116     c                             : '1' : rqsKsu : 'TT' : *blanks : 0
059400091116     c                             : tivssds : rpyEsito
059500091116     c                             )
059600091116     c*
059700091117     c* se cliente "stand-alone" abilitato => inserisco il record nel TIVTA
059800091117     c                   if        rpyEsito = *zeros         AND
059900091117     c                             tivssds.vssksu <> *blanks AND
060000091117     c                             tivssds.vssksu <> *zeros
060100091116     c                   exsr      scrvta
060200091116     c                   else
060300091127 xxx c***                add       99505         RWRINEW
060400091116     c                   endif
060500091116     c*
060600091116     c                   ENDSR
060700991110     c*--------------------------------------------------------------------------------------------*
060800090918 ttt c* imposta i clienti da elaborare
060900991110     c*--------------------------------------------------------------------------------------------*
061000090918     c     elacli        BEGSR
061100991110     c*
061200991110     c* reimposta variabili di lavoro
061300991110     c                   CLEAR                   sksc8                          clienti da elaborare
061400010409     c*
061500991110     c* controlla se il cliente è abilitato al TRACK & TRACING alla data corrente
061600090917     c                   CLEAR                   tivssds
061700090918     c
061800090918     c                   CALLP     Selettore_record_TIVSS('GETRCDVSS'
061900091126     c                             : '1' : ksui99 : 'TT' : *blanks : 0
062000090918     c                             : tivssds : rpyEsito
062100090918     c                             )
062200090917     c*
062300090917     c* se tutto ok => procedo
062400090917     c                   IF        rpyEsito = *zeros
062500090917     c*
062600090917     c* se richeista elaborazione di un unico cliente
062700091117     c                   IF        clii99 <> *blanks AND
062800091117     c                             clii99 <> *zeros
062900090917     c                   MOVEL     clii99        sksc8(1)
063000090917     c                   ELSE
063100090917     c*
063200091110     c* altrimenti, solo se presente legame, reperisco l'intera "familia"
063300091110     c                   IF        tivssds.vsstle <> *blanks
063400991110     c                   CLEAR                   tibs10ds
063500991110     c                   EVAL      d10drf = datcor                              *data riferimento
063600090917     c                   EVAL      d10tle = tivssds.vsstle                      *legame
063700991110     c                   EVAL      d10paf = 'F'                                 *cerca i figli
063800090917     c                   MOVE      ksui99        d10cod
063900991110     c                   CALL      'TIBS10R'                                    *cerca figli
064000991110     c                   PARM                    tibs10ds
064100090917     c                   IF        d10err <> ' '                                *SI errore: no figli
064200090917     c                   MOVEL     ksui99        sksc8(1)                       *cliente padre
064300090917     c                   ELSE                                                   *NO errore: si figli
064400090917     c                   Z-ADD     sksc11        sksc8                          *cliente padre+figli
064500090917     c                   ENDIF
064600091110     c*
064700091110     c                   ELSE
064800091110     c                   MOVEL     ksui99        sksc8(1)                       *cli. abilitato T&T
064900091110     c                   ENDIF
065000091110     c*
065100090917     c                   ENDIF
065200090917     c*
065300090917     c                   ENDIF
065400991110     c*
065500991110     c                   ENDSR
065600991110     c*--------------------------------------------------------------------------------------------*
065700090918 ttt c* cancella i dati da rielaborare (eventualmente)
065800991110     c*--------------------------------------------------------------------------------------------*
065900090918     c     delvta        BEGSR
066000010409     c*
066100010409     c* a seconda del tipo di elaborazione da effettuare...
066200090917     c                   IF        flgbol = '1'                                 ===> elab. x bolla
066300090917     c                   MOVEL     aasi99        ktadsp
066400090917     c                   MOVE      mgsi99        ktadsp
066500010409     c*
066600010409     c* lettura file -estratto bolle- per chiave bolla
066700090917     c     Keyvta_01     SETLL     tivta11l
066800090917     c     Keyvta_01     READE     tivta11l
066900090917     c                   DOW       NOT %eof(tivta11l)
067000090918     c*
067100090918     c* controlla record -estratto bolle-
067200090918     c                   EXSR      chkvta
067300090918     c*
067400090918     c* se record valido rispetto ai filtri su TIVTA
067500090918     c                   IF        $recok = 'S'
067600040517     c*
067700040518     c* cancella bolla anche nel file estensione
067800090917     C                   CLEAR                   TIS7F1DSI
067900040517     c                   EVAL      OpeIF1 = 'D'
068000040517     c                   EXSR      scrvtb
068100090917     c*
068200161118     c* cancello la bolla corrente dal TIVTA
068300161118     C/EXEC SQL
068400161118     C+ DELETE FROM TIVTA10F WHERE
068500161118     C+ VTALNP = :lnpi99 AND
068600161118     C+ VTANRS = :nrsi99 AND
068700161118     C+ VTANSP = :nspi99
068800161118     C/END-EXEC
068900091113     c*
069000091127 xxx c***                if        not %error
069100091127 xxx c***                add       1             RDELNEW
069200091127 xxx c***                endif
069300091113     c*
069400090918     c                   ENDIF
069500010409     c*
069600010409     c* lettura successiva file -estratto bolle-
069700090917     c     Keyvta_01     READE     tivta11l
069800090917     c                   ENDDO                                                  *fine estratto bolle
069900010409     c*
070000090917     c                   ELSE                                                   ===> elab. x KSU
070100091207     c*
070200091207     c* Se elaborazione x "tutto" KSU
070300091207     c                   if        flgcli = '0'
070400091207     C/EXEC SQL
070500091207     C+ DELETE FROM TIVTB00F WHERE VTBKSU = :ksui99
070600091207     C/END-EXEC
070700091207     c
070800091207     C/EXEC SQL
070900091207     C+ DELETE FROM TIVTA10F WHERE VTAKSU = :ksui99
071000091207     C/END-EXEC
071100091207     c                   else
071200010409     c*
071300991110     c* lettura file -estratto bolle- del cliente unificante richiesto
071400091116     c                   SETOFF                                       51
071500091116     c     Keyvta_13     SETLL     tivta13l
071600091116     c                   READ      tivta13l
071700091123     c                   DOW       NOT %eof(tivta13l) AND NOT *IN51
071800991110     c*
071900991110     c* controlla record -estratto bolle-
072000991110     c                   EXSR      chkvta
072100991110     c*
072200090917     c* se record valido rispetto ai filtri su TIVTA
072300090917     c                   IF        $recok = 'S'
072400040517     c*
072500040518     c* cancella bolla anche nel file estensione
072600090917     C                   CLEAR                   TIS7F1DSI
072700040517     c                   EVAL      OpeIF1 = 'D'
072800040517     c                   EXSR      scrvtb
072900090917     c*
073000090917     c* cancello il record corrente di TIVTA
073100091116     c                   DELETE(e) tivta013
073200091113     c*
073300091127 xxx c***                if        not %error
073400091127 xxx c***                add       1             RDELNEW
073500091127 xxx c***                endif
073600091113     c*
073700090917     c                   ENDIF
073800991110     c*
073900991110     c* lettura successiva file -estratto bolle-
074000091116     c                   READ      tivta13l
074100090917     c                   ENDDO                                                  *fine estratto bolle
074200091207     c*
074300091207     c                   endif
074400991110     c*
074500010409     c                   ENDIF
074600010409     c*
074700991110     c                   ENDSR
074800991110     c*--------------------------------------------------------------------------------------------*
074900090918 ttt c* controlla record -estratto bolle-
075000991110     c*--------------------------------------------------------------------------------------------*
075100090918     c     chkvta        BEGSR
075200991110     c*
075300991110     c                   MOVEL     'S'           $recok                         *record valido
075400991130     c*
075500090917     c* inserire qui eventuali filtri specifici sul TIVTA
075600091116     c*
075700091116     c* se codice cliente <> da quello richiesto, esclude
075800091117     c                   IF        clii99 <> *blanks AND
075900091117     c                             clii99 <> *zeros
076000091116     c                   IF        clii99 <> vtacli
076100091116     c                   MOVEL     'N'           $recok                         *record NON valido
076200091116     c                   ENDIF
076300091123     c                   IF        clii99 <  vtacli
076400091116     c                   SETON                                        51        *fine file
076500091116     c                   ENDIF
076600091116     c                   ENDIF
076700091116     c*
076800091116     c* se codice unificante <> da quello richiesto, esclude
076900091117     c                   IF        ksui99 <> *blanks AND
077000091117     c                             ksui99 <> *zeros
077100091116     c                   IF        ksui99 <> vtaksu
077200091116     c                   MOVEL     'N'           $recok                         *record NON valido
077300091116     c                   SETON                                        51        *fine file
077400091116     c                   ENDIF
077500091123     c                   IF        ksui99 <  vtaksu
077600091123     c                   SETON                                        51        *fine file
077700091123     c                   ENDIF
077800091116     c                   ENDIF
077900091116     c*
078000091116     c                   ENDSR
078100091116     c*--------------------------------------------------------------------------------------------*
078200091116 ttt c* scrive/cancella elenco colli in TIVTB00F
078300040514     c*--------------------------------------------------------------------------------------------*
078400090918     C     scrvtb        BEGSR
078500040517     c*
078600040517     c                   EVAL      LibIF1 = LibTIVTA
078700040517     C                   EVAL      KSUIF1 = vtaksu
078800040517     c                   MOVEL     vtadsp        AASIF1
078900040517     C                   EVAL      LnPIF1 = vtalnp
079000040517     C                   EVAL      NrSIF1 = vtanrs
079100040517     C                   EVAL      NSpIF1 = vtansp
079200040517     C                   EVAL      TBlIF1 = vtatbl
079300040514     C                   CLEAR                   TIS7F1DSO
079400091214 xxx c***                clear                   l_rwrinew
079500091214 xxx C***                CALL(E)   'TIS7F100R1'
079600091214     C                   CALL(E)   'TIS7F100R'
079700040514     C                   PARM                    TIS7F1DSI
079800040514     C                   PARM                    TIS7F1DSO
079900091214 xxx c***                parm                    l_rwrinew
080000040517     c*
080100040514     C                   ENDSR
080200091109     c*--------------------------------------------------------------------------------------------*
080300091109 ttt c* caricamento tabelle occorrenti
080400091109     c*--------------------------------------------------------------------------------------------*
080500091109     c     cartab        BEGSR
080600091109     c*---
080700091109     c* tipi legami "internet"
080800091109     c*---
080900091109     c                   z-add     *zeros        Giro              1 0
081000091109     c                   z-add     *zeros        jtle
081100091109     c                   for       Giro=1 to 2
081200091109     c*
081300091109     c                   movel     'TLE'         tbecod
081400091109     c     tbecod        setll     tntbe01l
081500091109     c     tbecod        reade     tntbe01l
081600091109     c                   dow       not %eof(tntbe01l)
081700091109     c                   setoff                                       50
081800091109     c                   if        tbeatb = *blanks
081900091109     c                   eval      dtle = tbeuni
082000091109     c* solo legami d tipo "internet"
082100091109     c                   if        §TLEGRP = 'W'
082200091109     c*
082300091109     c                   if        Giro=1
082400091109     c                   select
082500091109     c                   when      §TLEGRP = 'W' and tbeke1 = 'WW'
082600091109     c                   seton                                        50
082700091109     c                   endsl
082800091109     c                   endif
082900091109     c*
083000091109     c                   if        Giro=2
083100091109     c                   select
083200091109     c                   when      §TLEGRP = 'W' and tbeke1 <> 'WW'
083300091109     c                   seton                                        50
083400091109     c                   endsl
083500091109     c                   endif
083600091109     c*
083700091109     c                   if        *in50
083800091109     c                   add       1             jtle
083900091109     c                   eval      sktle(jtle) = tbeke1
084000091109     c                   endif
084100091109     c*
084200091109     c                   endif
084300091109     c                   endif
084400091109     c     tbecod        reade     tntbe01l
084500091109     c                   enddo
084600091109     c*
084700091109     c                   endfor
084800091109     c*
084900091109     c                   ENDSR
085000090921     c*--------------------------------------------------------------------------------------------*
085100090921 ttt c* gestione errori imprevisti di run-time
085200090921     c*--------------------------------------------------------------------------------------------*
085300091109     c     *pssr         BEGSR
085400090921     c*
085500090921     c* Visti che questo *pgm è chiamato in sincrona (call) anche dal trigger sul TITAS
085600090921     c* evito nel modo + assoluto che si possa "piantare"
085700091109     c                   MONITOR
085800090921     c*
085900091109     c                   MONITOR
086000090921     c*
086100091109     c                   DUMP(A)
086200091109     c                   EXSR      sndeml
086300090921     c*
086400090921     c* Su errore eseguo codice seguente
086500091109     c                   ON-ERROR
086600090921     c*
086700091109     c                   SETON                                        LR
086800091109     c                   RETURN
086900090921     c*
087000090921     c* Fine monitoraggio errori
087100091109     c                   ENDMON
087200090921     c*
087300090921     c* Su errore eseguo codice seguente
087400091109     c                   ON-ERROR
087500090921     c*
087600090921     c* Fine monitoraggio errori
087700091109     c                   ENDMON
087800090921     c*
087900091109     c                   ENDSR
088000090923     c*--------------------------------------------------------------------------------------------*
088100090923 ttt c* /TITLE Compongo il testo e spedisco una e-m@ail
088200090923     c*--------------------------------------------------------------------------------------------*
088300090923     c     sndeml        begsr
088400090923     c*
088500090923     c* Inizializzo variabili
088600090923     c                   movel     *blanks       wrkEml          253
088700090923     c                   movel     *blanks       wrkEmlMsg      5000
088800090923     c                   movel     *blanks       wrkEmlOgg        44
088900090923     c* Valorizzo i campi della e-m@ail
089000120306     c                   eval      wrkEml='cedalert@brt.it'
089100091109     c                   eval      wrkEmlOgg='CREA_TIVTA!!!'
089200090923     c                   EVAL      wrkEmlMsg = 'ERRORE IMPREVISTO IN '   +
089300090923     c                             'CREA_TIVTA'                          +':/N'+
089400090923     c                             ' '                                   +':/N'+
089500090923     c                             'Verificare dump del job: '           +':/N'+
089600091109     c                             %trim(JOB_NUM)+'/'+%trim(USER)+'/'          +
089700091109     c                             %trim(JOB_NAME)                       +':/N'+
089800091109     c                             ' '                                   +':/N'
089900090923     c*
090000090923     c                   call(e)   'TIS701C'
090100090923     c                   parm                    wrkEml
090200090923     c                   parm                    wrkEmlOgg
090300090923     c                   parm                    wrkEmlMsg
090400090923     c*
090500090923     c                   endsr
090600991102     c*--------------------------------------------------------------------------------------------*
090700090918 ttt c* operazioni iniziali
090800991102     c*--------------------------------------------------------------------------------------------*
090900991102     c     *inzsr        BEGSR
091000991102     c*
091100991102     c* ricevimento parametri
091200991104     c     *ENTRY        PLIST
091300991203     c                   PARM                    tis799dsI
091400091127 xxx c***                PARM                    RWRINEW
091500091127 xxx c***                PARM                    RDELNEW
091600091113     c*
091700091127 xxx c***                EVAL      RWRINEW = *zeros
091800091127 xxx c***                EVAL      RDELNEW = *zeros
091900091113     c*
092000010409     c* per prima cosa verifico i paramnetri ricevuti in input
092100091117     c                   IF        ksui99 <> *blanks AND
092200091117     c                             ksui99 <> *zeros
092300090918     c                   EVAL      flgbol = '0'                                 *elab. cliente unif.
092400010409     c                   ELSE
092500090918     c                   EVAL      flgbol = '1'                                 *elab. singola bolla
092600010409     c                   ENDIF
092700091207     c                   IF        clii99 <> *blanks AND
092800091207     c                             clii99 <> *zeros
092900091207     c                   EVAL      flgcli = '1'                                 *elab. cliente
093000091207     c                   ELSE
093100091207     c                   EVAL      flgcli = '0'                                 *elab. no x cliente
093200091207     c                   ENDIF
093300991102     c*
093400090918     c* chiavi di lettura
093500090918     c     Keytas_30     KLIST                                                  titas30c (unique)
093600090918     c                   KFLD                    aasi99                         -anno spedizione
093700090918     c                   KFLD                    lnpi99                         -linea partenza
093800090918     c                   KFLD                    nrsi99                         -serie
093900010409     c                   KFLD                    nspi99                         -spedizione
094000010409     c                   KFLD                    tbli99                         -tipo bolla
094100091014     c*
094200090918     c     Keyta4        KLIST                                                  tita430c/titaA30c
094300090918     c                   KFLD                    tasaas                         -anno spedizione
094400090918     c                   KFLD                    taslnp                         -linea partenza
094500090918     c                   KFLD                    tasnrs                         -serie
094600090918     c                   KFLD                    tasnsp                         -spedizione
094700090918     c                   KFLD                    ta4trc                         -tipo riferimento
094800091014     c*
094900090917     c     Keyvta_01     KLIST                                                  *tivta11l
095000090918     c                   KFLD                    ktadsp                          -data spedizione
095100090917     c                   KFLD                    lnpi99                          -linea partenza
095200090917     c                   KFLD                    nrsi99                          -serie
095300090917     c                   KFLD                    nspi99                          -spedizione
095400091014     c*
095500090918     c     Keyaco        KLIST                                                  *cnaco00f
095600090918     c                   KFLD                    acokut                          -utente
095700090918     c                   KFLD                    acokcc                          -capoconto
095800090918     c                   KFLD                    acoksc                          -sottoconto
095900091014     c*
096000091014     c     Keyvta_12     KLIST                                                  *tivta12l
096100091014     c                   KFLD                    vtaksu                          -cliente unificante
096200091014     c                   KFLD                    vtadsp                          -data spedizione
096300091014     c                   KFLD                    vtalnp                          -linea partenza
096400091014     c                   KFLD                    vtanrs                          -serie
096500091014     c                   KFLD                    vtansp                          -spedizione
096600091014     c                   KFLD                    vtatbl                          -tipo bolla
096700091116     c*
096800091116     c     Keyvta_13     KLIST                                                  *tivta12l
096900091116     c                   KFLD                    ksui99                          -cliente unificante
097000091116     c                   KFLD                    clii99                          -cliente KSC/CCM
097100161116     c*
097200161116     c     Keyblp_01     KLIST                                                  fnblp01l
097300161116     c                   KFLD                    aasi99                         -anno spedizione
097400161116     c                   KFLD                    lnpi99                         -linea partenza
097500161116     c                   KFLD                    nrsi99                         -serie
097600161116     c                   KFLD                    nspi99                         -spedizione
097700991102     c*
097800991102     c* reperimento data corrente
097900091106     C                   EVAL      datcor = %DEC(%DATE() : *ISO)
098000991126     c*
098100120306     c* imposta la data corrente - 2 mesi (per clienti BRT)
098200991126     c     *ISO          MOVE      datcor        wdata
098300991209     c     wdata         SUBDUR    2:*M          wdata
098400991126     c     *ISO          MOVE      wdata         datcorpre
098500991102     c*
098600991102     c* caricamento tabelle occorrenti
098700090918     c                   EXSR      cartab
098800991026     c*
098900991026     c                   ENDSR
