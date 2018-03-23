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
005100001002     f*---
005200001002     f* Anagrafiche contabili - clienti codificati
005300001002     f*---
005400020516     fcnaco00f  IF   E           K DISK                                         *Clienti codificati
005500991110     f*---
005600991117     f* Tabelle
005700991110     f*---
005800010409     Ftntbe01l  IF   E           K DISK                                         *Tabelle
005900991102     d*--------------------------------------------------------------------------------------------*
006000991102     d* Data structure
006100991102     d*--------------------------------------------------------------------------------------------*
006200991102     d*---
006300991102     d* Ds
006400090918     d*---
006500010409     d* reperimento clienti da cliente unificante
006600090918     d*---
006700991026     d tibs10ds      E DS
006800991026     d  sksc11                21   5520  0 DIM(500)                             *schiera clienti
006900090918     d*---
007000010409     d* DS riferimenti
007100090918     d*---
007200991102     d dta4a         E DS
007300070927     d dsbl4E        E DS
007400090918     d*---
007500010409     d* DS composizione data spedizione
007600090918     d*---
007700991026     d                 DS                  INZ
007800991026     d  tasaas                 1      4  0                                       -annp
007900991026     d  tasmgs                 5      8  0                                       -mese/giorno
008000991026     d tasdsp                  1      8  0                                      *data spedizione
008100090918     d*---
008200090918     d* DS Input di procedura
008300090918     d*---
008400991109     d tis799dsI     E DS
008500991126     d*---
008600991126     d* Variabili temporali
008700991126     d*---
008800991203     d                 DS
008900991126     d wdata                           D   DATFMT(*ISO) INZ(D'1999-01-01')
009000991102     d*---
009100991102     d* Variabili non riferite al data base
009200991102     d*---
009300991102     d n14             S             14  0                                      *Numerico 14
009400991102     d n8              S              8  0                                      *Numerico 8
009500010709     d n4              S              4  0                                      *Numerico 4
009600991102     d datcor          S              8  0                                      *data corrente a/m/g
009700030129     d datcorpre       S              8  0                                      *data limite cli BAR
009800991110     d wi              S              5  0                                      *indice di lavoro
009900991110     d wfkc            S              1                                         *tipo cliente (K/C)
010000010409     d flgbol          S              1                                         *flag x tipo elab.
010100091207     d flgcli          S              1                                         *flag x tipo elab.
010200010409     d depcli          S              8  0                                      *cod cli. x "cli"
010300991109     d*---
010400991109     d* Variabili di controllo
010500991109     d*---
010600991110     d $ftas           S              1    INZ('N')                             *fine lettura TITAS
010700991109     d $recok          S              1    INZ('N')                             *validità record
010800040514      *-------------------
010900040514      * DS lancio scrittura/cancellazione elenco dei colli in TIVTB00F
011000040514      *-------------------
011100040517     D TIS7F1DSI     E DS                  INZ
011200040514     D  OpeIF1       E
011300040517     D  LibIF1       E
011400040517     D  CmtCtlIF1    E                     INZ(*OFF)
011500040517     D  CommitIF1    E                     INZ(*OFF)
011600040514     D TIS7F1DSO     E DS
011700040514     D                                     INZ
011800040514     D InfTIVTA        DS
011900090918     D  LibTIVTA              93    102
012000090918     D*
012100090918     D tivssds       E DS                  EXTNAME(tivss00f) QUALIFIED
012200090918     D dtle          E DS
012300090918     D sktle           S              2    DIM(20)
012400090918     D sksc8           S              8  0 DIM(500)                             *clienti
012500090918     D jtle            S              5  0                                      *indici
012600090918     D Tascli          S                   LIKE(tasksc)                         *lett. titas31/33c
012700090918     D ktadsp          S                   LIKE(vtadsp)                         *lettura tivta10f
012800091207 xxx D*** RWRINEW         S              6  0
012900091207 xxx D*** RDELNEW         S              6  0
013000090918     D*
013100090918     D/COPY GAITRASRC/SRCPROTOPR,TIS7701R
013200091124     D/COPY GAITRASRC/SRCPROTOPI,TIS7701R
013300090918     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
013400090918     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
013500090921     D*
013600090921     D pgmstatus      SDS
013700090921     D  JOB_NAME             244    253                                         * Job name
013800090921     D  USER                 254    263                                         * User name
013900090921     D  JOB_NUM              264    269                                         * Job number
014000090918     c*--------------------------------------------------------------------------------------------*
014100090918     c* Main lines
014200991102     c*--------------------------------------------------------------------------------------------*
014300091207     C*
014400091207     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
014500091207     C
014600091207     C/EXEC SQL
014700091207     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
014800091207     C/END-EXEC
014900091207     C*
015000091207
015100991110     c*
015200991110     c* cancella l'estratto bolle del CLIENTE UNIFICANTE
015300010409     c                   EXSR      delvta
015400010409     c*
015500010409     c* a seconda dei parametri ricevuti in ingresso => eseguo differenti elaborazioni
015600010409     c                   IF        flgbol = '0'
015700010409     c*
015800010409     c* imposta i clienti da elaborare
015900010409     c                   EXSR      elacli
016000991109     c*
016100991109     c* elabora tutti i cienti memorizzati
016200090917     c     1             DO        500           wi
016300090917     c                   IF        sksc8(wi) > *zeros
016400991110     c*
016500991110     c* elabora le bolle per CLIENTE di FATTURAZIONE
016600090918     c                   EVAL      wfkc = 'K'                                   cliente fatturazione
016700090918     c                   EXSR      elatas                                       *elabora bolle
016800991110     c*
016900991110     c* elabora le bolle per CLIENTE MITTENTE
017000090918     c                   EVAL      wfkc = 'C'                                   cliente mittente
017100090918     c                   EXSR      elatas                                       *elabora bolle
017200090918     c*
017300090918     c                   ELSE
017400090918     c                   LEAVE
017500090917     c                   ENDIF
017600090917     c                   ENDDO                                                  *fine clienti
017700010409     c*
017800010409     c                   ELSE
017900010409     c*
018000010409     c* effettua considerazioni/elaborazioni x singola bolla
018100090918     c                   EXSR      elabol
018200010409     c*
018300010409     c                   ENDIF
018400010409     c*
018500091214 xxx c***                eval      l_SUNI99 = SUNI99
018600091214 xxx c***                eval      l_KSUI99 = KSUI99
018700091214 xxx c***                eval      l_CLII99 = CLII99
018800091214 xxx c***                eval      l_AASI99 = AASI99
018900091214 xxx c***                eval      l_MGSI99 = MGSI99
019000091214 xxx c***                eval      l_LNPI99 = LNPI99
019100091214 xxx c***                eval      l_NRSI99 = NRSI99
019200091214 xxx c***                eval      l_NSPI99 = NSPI99
019300091214 xxx c***                eval      l_TBLI99 = TBLI99
019400091214 xxx c***                eval      l_OPEI99 = OPEI99
019500091214 xxx c***                eval      l_LOGDATA = datcor
019600091214 xxx c***                write(e)  tivtalog
019700091207     c*
019800991110     c                   SETON                                            lr
019900991110     c*--------------------------------------------------------------------------------------------*
020000090918 ttt c* elabora le bolle per CLIENTE di FATTURAZIONE oppure CLIENTE MITTENTE
020100991110     c*--------------------------------------------------------------------------------------------*
020200991110     c     elatas        BEGSR
020300991110     c*
020400991110     c* posizionamento e prima lettura -bolle-
020500991110     c                   EXSR      settas
020600991110     c*
020700991110     c* ciclo fino a fine file
020800090917     c                   DOW       $ftas='N'
020900991110     c*
021000991110     c* memorizza i dati nel file -estratto bolle-
021100090918     c                   EXSR      scrvta
021200991110     c*
021300991110     c* lettura successiva -bolle-
021400991110     c                   EXSR      lettas
021500090917     c                   ENDDO
021600991110     c*
021700991110     c                   ENDSR
021800991110     c*--------------------------------------------------------------------------------------------*
021900090917 ttt c* posizionamento e prima lettura -bolle-
022000991110     c*--------------------------------------------------------------------------------------------*
022100991110     c     settas        BEGSR
022200991110     c*
022300991110     c* reimposta variabili di lavoro
022400991110     c                   EVAL      $ftas = 'N'
022500991110     c*
022600991110     c* si posiziona x CLIENTE di FATTURAZIONE (1° giro) oppure x CLIENTE MITTENTE (2° giro)
022700090917     c                   MOVE      sksc8(wi)     Tascli
022800090918     c                   IF        wfkc = 'K'                                   *cliente fattur.ne
022900090917     c     Tascli        SETLL     titas31c
023000090917     c                   ELSE                                                   *cliente mittente
023100090917     c     Tascli        SETLL     titas33c
023200090917     c                   ENDIF
023300090917     c                   IF        %equal                                       *no fine file
023400090917     c                   EXSR      lettas                                       *lettura record
023500090917     c                   ELSE                                                   *fine file
023600090917     c                   MOVEL     'S'           $ftas                          *fine bolle
023700090917     c                   ENDIF
023800991110     c*
023900991110     c                   ENDSR
024000991110     c*--------------------------------------------------------------------------------------------*
024100090918 ttt c* lettura successiva -bolle-
024200991110     c*--------------------------------------------------------------------------------------------*
024300090918     c     lettas        BEGSR
024400090917     c*
024500991110     c* legge x CLIENTE di FATTURAZIONE (1° giro) oppure x CLIENTE MITTENTE (2° giro)
024600991110     c                   MOVEL     'N'           $recok
024700090917     c     $ftas         DOWEQ     'N'
024800090929     c     $recok        ANDEQ     'N'
024900090917     c                   IF        wfkc = 'K'                                   *cliente fattur.ne
025000091117     c     Tascli        READE     titas31c
025100091117     c                   Z-ADD     tasksc        depcli
025200090917     c                   ELSE                                                   *cliente mittente
025300091117     c     Tascli        READE     titas33c
025400091117     c                   Z-ADD     tasccm        depcli
025500090917     c                   ENDIF
025600090923     c                   IF        %eof                                         *fine file
025700090917     c                   MOVEL     'S'           $ftas                          *fine bolle
025800090917     c                   ELSE
025900090917     c                   EXSR      chktas                                       *controlla record
026000090917     c                   ENDIF
026100090917     c                   ENDDO
026200991110     c*
026300991110     c                   ENDSR
026400991110     c*--------------------------------------------------------------------------------------------*
026500090918 ttt c* controlla record -bolle-
026600090918     c*--------------------------------------------------------------------------------------------*
026700090918     c     chktas        BEGSR
026800991110     c*
026900991110     c                   MOVEL     'S'           $recok                         *record valido
027000991130     C*
027100000105     c* data spedizione compresa fra data corrente e data corrente - 2 mesi, altrimenti esclude
027200090917     c                   IF        $recok = 'S'                                 *record valido
027300090917     c                   IF        tasdsp >= datcorpre  AND
027400090917     c                             tasdsp <= datcor
027500090917     c                   ELSE
027600090917     c                   MOVEL     'N'           $recok                         *record NON valido
027700090917     c                   ENDIF
027800090917     c                   ENDIF
027900090917     c*
028000090917     c* se lettura x CLIENTE FATTURAZ. esclude le bolle con cliente mittente = cliente fatturazione
028100090917     c                   IF        $recok = 'S'                                 *record valido
028200090918     c                   IF        wfkc = 'K'
028300090917     c                   IF        tasksc = tasccm
028400090917     c                   MOVEL     'N'           $recok                         *record NON valido
028500090917     c                   ENDIF
028600090917     c                   ENDIF
028700090917     c                   ENDIF
028800090917     c
028900090917     c* se bolla di recupero, esclude
029000090917     c                   IF        $recok = 'S'                                 *record valido
029100090917     c                   IF        tastbl = 'AR' OR                             *NO bolla RECUPERO
029200000124     c                             tastbl = 'A3' OR
029300000124     c                             tastbl = 'F3' OR
029400000124     c                             tastbl = 'AP' OR
029500000124     c                             tastbl = 'FC'
029600000120     c                   MOVEL     'N'           $recok                         *record NON valido
029700090917     c                   ENDIF
029800090917     c                   ENDIF
029900090917     c
030000090917     c*
030100000105     c* se bolla figlia, esclude
030200090917     c                   IF        $recok = 'S'                                 *record valido
030300090917     c                   MOVEL     *blanks       wEsito1           1
030400090917     c                   EVAL      wEsito1 = %char(UBLBLSPE_GetLblTyp(
030500090917     c                                                tasaas
030600090917     c                                               :taslnp
030700090917     c                                               :tasnrs
030800090917     c                                               :tasnsp
030900090917     c                                               :pOutLblTyp
031000090917     c                                               :pOutAnnoBO
031100090917     c                                               :pOutLineaParBO
031200090917     c                                               :pOutSerieBO
031300090917     c                                               :pOutNumSpedBO
031400090917     c                                               :pOutDcmBO
031500090917     c                                               :pOutCcaBO
031600090917     c                                               :pOutRblBO))
031700090917     c                   IF        pOutLblTyp = 'O'
031800090917     c                   ELSE
031900000105     c                   MOVEL     'N'           $recok                         *record NON valido
032000090917     c                   ENDIF
032100090917     c                   ENDIF
032200991110     c*
032300991110     c                   ENDSR
032400090917     c*--------------------------------------------------------------------------------------------*
032500090918 ttt c* memorizza i dati nel file -estratto bolle-
032600991110     c*--------------------------------------------------------------------------------------------*
032700090918     c     scrvta        BEGSR
032800040517     c*
032900091022     c                   EXSR      impkeyvta                                    *imposta la key
033000091014     c*
033100091014     c* verifico se chiave univoca già presente..
033200091022     c     Keyvta_12     setll     tivta12l
033300091014     c                   if        %equal(tivta12l)
033400091014     c                   else
033500091022     c                   EXSR      impvta                                       *imposta i campi
033600090917     c                   WRITE(e)  tivta000                                     *scrive record
033700091113     c*
033800091127 xxx c***                if        not %error
033900091127 xxx c***                add       1             RWRINEW
034000091127 xxx c***                endif
034100091113     c*
034200040517     c* scrive bolla anche nel file estensione
034300090917     C                   CLEAR                   TIS7F1DSI
034400040517     c                   EVAL      OpeIF1 = 'W'
034500040517     c                   EXSR      scrvtb
034600991110     c*
034700091022     c                   endif
034800091022     c*
034900090918     c                   ENDSR
035000091022     c*--------------------------------------------------------------------------------------------*
035100091022 ttt c* imposta campi chiave di TIVTA per la scrittura
035200091022     c*--------------------------------------------------------------------------------------------*
035300091022     c     impkeyvta     BEGSR
035400091022     c*
035500091022     c* imposta chiave
035600091022     c                   EVAL      vtaksu = tivssds.vssksu                      *cliente unificante
035700091022     c                   EVAL      vtadsp = tasdsp                              *data spedizione
035800091022     c                   EVAL      vtalnp = taslnp                              *linea partenza
035900091022     c                   EVAL      vtanrs = tasnrs                              *serie
036000091022     c                   EVAL      vtansp = tasnsp                              *spedizione
036100091022     c                   EVAL      vtatbl = tastbl                              *tipo bolla
036200091022     c*
036300091022     c                   ENDSR
036400991026     c*--------------------------------------------------------------------------------------------*
036500090918 ttt c* imposta campi di TIVTA per la scrittura
036600991026     c*--------------------------------------------------------------------------------------------*
036700090918     c     impvta        BEGSR
036800091022     c*
036900090918     c*
037000090918     c* <--- inserimento logica x reperimento SUBK ----->
037100091014     c*
037200091014 xxx c*                  EVAL      vtasubk = %trim(xxx)
037300091014     c*
037400090918     c* <----------------------------------------------->
037500090918     c*
037600091117     c                   MOVEL     depcli        vtacli                         *cliente KSC/CCM
037700090929     c                   EVAL      vtafkc = wfkc                                *tipo cliente
037800991110     c*
037900001002     c* imposta campi da TITAS
038000991110     c                   EVAL      vtarmn = tasrmn                              *riferim mittente
038100091014     c                   EVAL      vtarsd = %trim(tasrsd)                       *destinatario
038200001002     c                   EVAL      vtapoa = taslna                              *punto op. di arrivo
038300001002     c                   EVAL      vtaprd = tasprd                              *provincia di dest.
038400001002     c                   IF        tasfgc = 'S'
038500001002     c                   EVAL      vtagia = '1'                                 *stato giacenza "SI"
038600001002     c                   ELSE
038700001002     c                   EVAL      vtagia = '2'                                 *stato giacenza "?"
038800001002     c                   ENDIF
038900001002     c*
039000091014     c                   EVAL      vtalna = %trim(taslod) +' ' + tasnzd         *destinazione
039100001002     c*
039200090917     c                   IF        tasdcm > *zeros                              *consegnata
039300090917     c                   EVAL      vtasta = '1'                                 *stato consegnata
039400090917     c                   ELSE                                                   *non consegnata
039500090917     c                   EVAL      vtasta = '2'                                 *stato ocnsega
039600090917     c                   ENDIF
039700090917     c*
039800090917     c* imposta la ragione sociale del cliente mittente
039900091014     c                   clear                   vtarsm
040000090917     c                   MOVE      tasccm        n4
040100090917     c                   IF        n4 <> 8888                                   *cl NON codificato
040200090918     c                   movel     1             acokut
040300090918     c                   z-add     151           acokcc
040400090918     c                   z-add     tasccm        acoksc
040500090918     c     Keyaco        CHAIN     cnaco00f
040600090918     c                   IF        %found(cnaco00f)
040700091014     c                   eval      vtarsm = %trim(acorag)
040800090917     c                   ENDIF
040900090917     c                   ELSE                                                   *cl codificato
041000090918     c                   MOVEL     'M'           ta4trc
041100090918     c     Keyta4        CHAIN     titaA30c
041200090918     c                   IF        %found(titaA30c)
041300091014     c                   eval      vtarsm = %trim(taarsc)
041400090917     c                   ENDIF
041500090917     c                   ENDIF
041600090917     c*
041700991110     c* imposta campi da TITA4
041800991102     c                   CLEAR                   dta4a
041900090918     c                   MOVEL     'A'           ta4trc
042000090918     c     Keyta4        CHAIN     tita430c
042100090918     c                   IF        %found(tita430c)
042200991102     c                   MOVEL     ta4not        dta4a
042300091014     c                   EVAL      vtarma = %trim(§ta4arma)                     *riferim alfabetico
042400011105     c****
042500050124     c* Personalizzazione HERBALIFE --> prende 8 char del riferimento alfabetico e li
042600011105     c*                                 mette nel campo VTAA08 (per questo cliente è il N° ORDINE)
042700011105     c****
042800110721     c                   IF        vtaksu = '00890896' or
042900110721     c                             vtaksu = '01660912'
043000091014     c                   EVAL      vtaa08 = %TRIM(%SUBST(§ta4arma:3:8))
043100090917     c                   ENDIF
043200090917     c*
043300090917     c                   ELSE                                                   *non trovato
043400090917     c*
043500091014     c                   CLEAR                   vtarma
043600091014     c                   CLEAR                   vtaa08
043700090917     c                   ENDIF
043800070927     c
043900070927     c* Riferimento partner estero
044000070927     c                   CLEAR                   dsbl4e
044100090918     c                   MOVEL     'E'           ta4trc
044200090918     c     Keyta4        CHAIN     tita430c
044300090918     c                   IF        %found(tita430c)
044400070927     c                   MOVEL     ta4not        dsbl4e
044500091014     c                   eval      vtarpt = %trim(§b4erp)
044600070927     c                   else
044700091014     c                   clear                   vtarpt
044800070927     c                   endif
044900991026     c*
045000991026     c                   ENDSR
045100010409     c*--------------------------------------------------------------------------------------------*
045200090918 ttt c* verifica se la bolla è da elaborare
045300010409     c*--------------------------------------------------------------------------------------------*
045400010409     c     elabol        BEGSR
045500010409     c*
045600090918     c* per prima cosa "chaino" la bolla corrente
045700090918     c     Keytas_30     chain     titas30c
045800010529     c                   if        %found(titas30c)
045900090918     c*
046000090918     c                   eval      wfkc = *blanks                               *elab. x bolla
046100090918     c*
046200090918     c* verifica se è una bolla ba considerare
046300090918     c                   exsr      chktas
046400090918     c                   if        $recok = 'S'
046500090918     c*
046600090918     c* se il cliente di fatturazione ed il cliente mittente sulla bolla sono diversi...
046700090918     c* tento di inserire in TIVTA x CCM
046800090918     c                   if        tasccm <> tasksc
046900090918     c                   z-add     tasccm        depcli
047000091116     c                   exsr      rtvStAlone
047100091116     c                   exsr      rtvFamily
047200090918     c                   endif
047300090918     c*
047400090918     c* x KSC tento d inserire in TIVTA sempre e cmq
047500090918     c                   z-add     tasksc        depcli
047600091116     c                   exsr      rtvStAlone
047700090918     c                   exsr      rtvFamily
047800090918     c*
047900090918     c                   endif
048000090918     c                   endif
048100010424     c*
048200010409     c                   ENDSR
048300090918     c*--------------------------------------------------------------------------------------------*
048400090918 ttt c* reperisce la "familia" x tutti i tipi legami d tipo "internet"
048500090918     c*--------------------------------------------------------------------------------------------*
048600090918     c     rtvFamily     BEGSR
048700090918     c*
048800090918     c* ciclo x tutti i legami d tipo "internet"
048900090918     c                   z-add     1             jtle
049000090918     c                   dow       jtle <= %elem(sktle)
049100090918     c                   if        sktle(jtle) <> *blanks
049200090918     c*
049300090918     c                   clear                   sksc8                          *clienti da elab.
049400091106     c* cerco il padre
049500090918     c                   CLEAR                   tibs10ds
049600090918     c                   EVAL      d10drf = datcor                              *data riferimento
049700091020     c                   EVAL      d10tle = sktle(jtle)                         *legame
049800090918     c                   EVAL      d10paf = 'P'                                 *cerca il padre
049900090918     c                   Z-ADD     depcli        d10cod
050000090918     c                   CALL      'TIBS10R'                                    *cerca figli
050100090918     c                   PARM                    tibs10ds
050200091123     c                   IF        d10err = ' '                                 *NO errore: si figli
050300090918     c                   Z-ADD     d10cop        sksc8(1)                       *cliente padre
050400090918     c*
050500090918     c* controlla se il padre reperito è abilitato al TRACK & TRACING alla data corrente
050600090918     c                   MOVE      sksc8(1)      rqsKsu
050700090918     c                   CLEAR                   tivssds
050800090918     c
050900090918     c                   CALLP     Selettore_record_TIVSS('GETRCDVSS'
051000091022     c                             : '1' : rqsKsu : 'TT' : *blanks : 0
051100090918     c                             : tivssds : rpyEsito
051200090918     c                             )
051300090918     c*
051400091116     c* se padre abilitato => inserisco il record nel TIVTA
051500091117     c                   if        rpyEsito = *zeros         AND
051600091117     c                             tivssds.vssksu <> *blanks AND
051700091117     c                             tivssds.vssksu <> *zeros
051800091117     c                   if        tivssds.vsstle = sktle(jtle)
051900090918     c                   exsr      scrvta
052000091109     c                   endif
052100091109     c                   else
052200091127 xxx c***                add       99505         RWRINEW
052300091109     c                   endif
052400091123     c*
052500091123     c                   ENDIF
052600090918     c*
052700090918     c                   else
052800090918     c                   leave
052900090918     c                   endif
053000090918     c*
053100090918     c                   add       1             jtle
053200090918     c                   enddo
053300090918     c*
053400090918     c                   ENDSR
053500091116     c*--------------------------------------------------------------------------------------------*
053600091116 ttt c* tenta d elaborare anche x codice cliente "stand alone"
053700091116     c*--------------------------------------------------------------------------------------------*
053800091116     c     rtvStAlone    BEGSR
053900091116     c*
054000091116     c                   clear                   sksc8                          *clienti da elab.
054100091116     c                   Z-ADD     depcli        sksc8(1)                       *cliente corrente
054200091116     c*
054300091116     c* controlla se il cliente "stand-alone" è abilitato al TRACK & TRACING alla data corrente
054400091116     c                   MOVE      sksc8(1)      rqsKsu
054500091116     c                   CLEAR                   tivssds
054600091116     c
054700091116     c                   CALLP     Selettore_record_TIVSS('GETRCDVSS'
054800091116     c                             : '1' : rqsKsu : 'TT' : *blanks : 0
054900091116     c                             : tivssds : rpyEsito
055000091116     c                             )
055100091116     c*
055200091117     c* se cliente "stand-alone" abilitato => inserisco il record nel TIVTA
055300091117     c                   if        rpyEsito = *zeros         AND
055400091117     c                             tivssds.vssksu <> *blanks AND
055500091117     c                             tivssds.vssksu <> *zeros
055600091116     c                   exsr      scrvta
055700091116     c                   else
055800091127 xxx c***                add       99505         RWRINEW
055900091116     c                   endif
056000091116     c*
056100091116     c                   ENDSR
056200991110     c*--------------------------------------------------------------------------------------------*
056300090918 ttt c* imposta i clienti da elaborare
056400991110     c*--------------------------------------------------------------------------------------------*
056500090918     c     elacli        BEGSR
056600991110     c*
056700991110     c* reimposta variabili di lavoro
056800991110     c                   CLEAR                   sksc8                          clienti da elaborare
056900010409     c*
057000991110     c* controlla se il cliente è abilitato al TRACK & TRACING alla data corrente
057100090917     c                   CLEAR                   tivssds
057200090918     c
057300090918     c                   CALLP     Selettore_record_TIVSS('GETRCDVSS'
057400091126     c                             : '1' : ksui99 : 'TT' : *blanks : 0
057500090918     c                             : tivssds : rpyEsito
057600090918     c                             )
057700090917     c*
057800090917     c* se tutto ok => procedo
057900090917     c                   IF        rpyEsito = *zeros
058000090917     c*
058100090917     c* se richeista elaborazione di un unico cliente
058200091117     c                   IF        clii99 <> *blanks AND
058300091117     c                             clii99 <> *zeros
058400090917     c                   MOVEL     clii99        sksc8(1)
058500090917     c                   ELSE
058600090917     c*
058700091110     c* altrimenti, solo se presente legame, reperisco l'intera "familia"
058800091110     c                   IF        tivssds.vsstle <> *blanks
058900991110     c                   CLEAR                   tibs10ds
059000991110     c                   EVAL      d10drf = datcor                              *data riferimento
059100090917     c                   EVAL      d10tle = tivssds.vsstle                      *legame
059200991110     c                   EVAL      d10paf = 'F'                                 *cerca i figli
059300090917     c                   MOVE      ksui99        d10cod
059400991110     c                   CALL      'TIBS10R'                                    *cerca figli
059500991110     c                   PARM                    tibs10ds
059600090917     c                   IF        d10err <> ' '                                *SI errore: no figli
059700090917     c                   MOVEL     ksui99        sksc8(1)                       *cliente padre
059800090917     c                   ELSE                                                   *NO errore: si figli
059900090917     c                   Z-ADD     sksc11        sksc8                          *cliente padre+figli
060000090917     c                   ENDIF
060100091110     c*
060200091110     c                   ELSE
060300091110     c                   MOVEL     ksui99        sksc8(1)                       *cli. abilitato T&T
060400091110     c                   ENDIF
060500091110     c*
060600090917     c                   ENDIF
060700090917     c*
060800090917     c                   ENDIF
060900991110     c*
061000991110     c                   ENDSR
061100991110     c*--------------------------------------------------------------------------------------------*
061200090918 ttt c* cancella i dati da rielaborare (eventualmente)
061300991110     c*--------------------------------------------------------------------------------------------*
061400090918     c     delvta        BEGSR
061500010409     c*
061600010409     c* a seconda del tipo di elaborazione da effettuare...
061700090917     c                   IF        flgbol = '1'                                 ===> elab. x bolla
061800090917     c                   MOVEL     aasi99        ktadsp
061900090917     c                   MOVE      mgsi99        ktadsp
062000010409     c*
062100010409     c* lettura file -estratto bolle- per chiave bolla
062200090917     c     Keyvta_01     SETLL     tivta11l
062300090917     c     Keyvta_01     READE     tivta11l
062400090917     c                   DOW       NOT %eof(tivta11l)
062500090918     c*
062600090918     c* controlla record -estratto bolle-
062700090918     c                   EXSR      chkvta
062800090918     c*
062900090918     c* se record valido rispetto ai filtri su TIVTA
063000090918     c                   IF        $recok = 'S'
063100040517     c*
063200040518     c* cancella bolla anche nel file estensione
063300090917     C                   CLEAR                   TIS7F1DSI
063400040517     c                   EVAL      OpeIF1 = 'D'
063500040517     c                   EXSR      scrvtb
063600090917     c*
063700090917     c* cancello il record corrente di TIVTA
063800091109     c                   DELETE(e) tivta011
063900091113     c*
064000091127 xxx c***                if        not %error
064100091127 xxx c***                add       1             RDELNEW
064200091127 xxx c***                endif
064300091113     c*
064400090918     c                   ENDIF
064500010409     c*
064600010409     c* lettura successiva file -estratto bolle-
064700090917     c     Keyvta_01     READE     tivta11l
064800090917     c                   ENDDO                                                  *fine estratto bolle
064900010409     c*
065000090917     c                   ELSE                                                   ===> elab. x KSU
065100091207     c*
065200091207     c* Se elaborazione x "tutto" KSU
065300091207     c                   if        flgcli = '0'
065400091207     C/EXEC SQL
065500091207     C+ DELETE FROM TIVTB00F WHERE VTBKSU = :ksui99
065600091207     C/END-EXEC
065700091207     c
065800091207     C/EXEC SQL
065900091207     C+ DELETE FROM TIVTA10F WHERE VTAKSU = :ksui99
066000091207     C/END-EXEC
066100091207     c                   else
066200010409     c*
066300991110     c* lettura file -estratto bolle- del cliente unificante richiesto
066400091116     c                   SETOFF                                       51
066500091116     c     Keyvta_13     SETLL     tivta13l
066600091116     c                   READ      tivta13l
066700091123     c                   DOW       NOT %eof(tivta13l) AND NOT *IN51
066800991110     c*
066900991110     c* controlla record -estratto bolle-
067000991110     c                   EXSR      chkvta
067100991110     c*
067200090917     c* se record valido rispetto ai filtri su TIVTA
067300090917     c                   IF        $recok = 'S'
067400040517     c*
067500040518     c* cancella bolla anche nel file estensione
067600090917     C                   CLEAR                   TIS7F1DSI
067700040517     c                   EVAL      OpeIF1 = 'D'
067800040517     c                   EXSR      scrvtb
067900090917     c*
068000090917     c* cancello il record corrente di TIVTA
068100091116     c                   DELETE(e) tivta013
068200091113     c*
068300091127 xxx c***                if        not %error
068400091127 xxx c***                add       1             RDELNEW
068500091127 xxx c***                endif
068600091113     c*
068700090917     c                   ENDIF
068800991110     c*
068900991110     c* lettura successiva file -estratto bolle-
069000091116     c                   READ      tivta13l
069100090917     c                   ENDDO                                                  *fine estratto bolle
069200091207     c*
069300091207     c                   endif
069400991110     c*
069500010409     c                   ENDIF
069600010409     c*
069700991110     c                   ENDSR
069800991110     c*--------------------------------------------------------------------------------------------*
069900090918 ttt c* controlla record -estratto bolle-
070000991110     c*--------------------------------------------------------------------------------------------*
070100090918     c     chkvta        BEGSR
070200991110     c*
070300991110     c                   MOVEL     'S'           $recok                         *record valido
070400991130     c*
070500090917     c* inserire qui eventuali filtri specifici sul TIVTA
070600091116     c*
070700091116     c* se codice cliente <> da quello richiesto, esclude
070800091117     c                   IF        clii99 <> *blanks AND
070900091117     c                             clii99 <> *zeros
071000091116     c                   IF        clii99 <> vtacli
071100091116     c                   MOVEL     'N'           $recok                         *record NON valido
071200091116     c                   ENDIF
071300091123     c                   IF        clii99 <  vtacli
071400091116     c                   SETON                                        51        *fine file
071500091116     c                   ENDIF
071600091116     c                   ENDIF
071700091116     c*
071800091116     c* se codice unificante <> da quello richiesto, esclude
071900091117     c                   IF        ksui99 <> *blanks AND
072000091117     c                             ksui99 <> *zeros
072100091116     c                   IF        ksui99 <> vtaksu
072200091116     c                   MOVEL     'N'           $recok                         *record NON valido
072300091116     c                   SETON                                        51        *fine file
072400091116     c                   ENDIF
072500091123     c                   IF        ksui99 <  vtaksu
072600091123     c                   SETON                                        51        *fine file
072700091123     c                   ENDIF
072800091116     c                   ENDIF
072900091116     c*
073000091116     c                   ENDSR
073100091116     c*--------------------------------------------------------------------------------------------*
073200091116 ttt c* scrive/cancella elenco colli in TIVTB00F
073300040514     c*--------------------------------------------------------------------------------------------*
073400090918     C     scrvtb        BEGSR
073500040517     c*
073600040517     c                   EVAL      LibIF1 = LibTIVTA
073700040517     C                   EVAL      KSUIF1 = vtaksu
073800040517     c                   MOVEL     vtadsp        AASIF1
073900040517     C                   EVAL      LnPIF1 = vtalnp
074000040517     C                   EVAL      NrSIF1 = vtanrs
074100040517     C                   EVAL      NSpIF1 = vtansp
074200040517     C                   EVAL      TBlIF1 = vtatbl
074300040514     C                   CLEAR                   TIS7F1DSO
074400091214 xxx c***                clear                   l_rwrinew
074500091214 xxx C***                CALL(E)   'TIS7F100R1'
074600091214     C                   CALL(E)   'TIS7F100R'
074700040514     C                   PARM                    TIS7F1DSI
074800040514     C                   PARM                    TIS7F1DSO
074900091214 xxx c***                parm                    l_rwrinew
075000040517     c*
075100040514     C                   ENDSR
075200091109     c*--------------------------------------------------------------------------------------------*
075300091109 ttt c* caricamento tabelle occorrenti
075400091109     c*--------------------------------------------------------------------------------------------*
075500091109     c     cartab        BEGSR
075600091109     c*---
075700091109     c* tipi legami "internet"
075800091109     c*---
075900091109     c                   z-add     *zeros        Giro              1 0
076000091109     c                   z-add     *zeros        jtle
076100091109     c                   for       Giro=1 to 2
076200091109     c*
076300091109     c                   movel     'TLE'         tbecod
076400091109     c     tbecod        setll     tntbe01l
076500091109     c     tbecod        reade     tntbe01l
076600091109     c                   dow       not %eof(tntbe01l)
076700091109     c                   setoff                                       50
076800091109     c                   if        tbeatb = *blanks
076900091109     c                   eval      dtle = tbeuni
077000091109     c* solo legami d tipo "internet"
077100091109     c                   if        §TLEGRP = 'W'
077200091109     c*
077300091109     c                   if        Giro=1
077400091109     c                   select
077500091109     c                   when      §TLEGRP = 'W' and tbeke1 = 'WW'
077600091109     c                   seton                                        50
077700091109     c                   endsl
077800091109     c                   endif
077900091109     c*
078000091109     c                   if        Giro=2
078100091109     c                   select
078200091109     c                   when      §TLEGRP = 'W' and tbeke1 <> 'WW'
078300091109     c                   seton                                        50
078400091109     c                   endsl
078500091109     c                   endif
078600091109     c*
078700091109     c                   if        *in50
078800091109     c                   add       1             jtle
078900091109     c                   eval      sktle(jtle) = tbeke1
079000091109     c                   endif
079100091109     c*
079200091109     c                   endif
079300091109     c                   endif
079400091109     c     tbecod        reade     tntbe01l
079500091109     c                   enddo
079600091109     c*
079700091109     c                   endfor
079800091109     c*
079900091109     c                   ENDSR
080000090921     c*--------------------------------------------------------------------------------------------*
080100090921 ttt c* gestione errori imprevisti di run-time
080200090921     c*--------------------------------------------------------------------------------------------*
080300091109     c     *pssr         BEGSR
080400090921     c*
080500090921     c* Visti che questo *pgm è chiamato in sincrona (call) anche dal trigger sul TITAS
080600090921     c* evito nel modo + assoluto che si possa "piantare"
080700091109     c                   MONITOR
080800090921     c*
080900091109     c                   MONITOR
081000090921     c*
081100091109     c                   DUMP(A)
081200091109     c                   EXSR      sndeml
081300090921     c*
081400090921     c* Su errore eseguo codice seguente
081500091109     c                   ON-ERROR
081600090921     c*
081700091109     c                   SETON                                        LR
081800091109     c                   RETURN
081900090921     c*
082000090921     c* Fine monitoraggio errori
082100091109     c                   ENDMON
082200090921     c*
082300090921     c* Su errore eseguo codice seguente
082400091109     c                   ON-ERROR
082500090921     c*
082600090921     c* Fine monitoraggio errori
082700091109     c                   ENDMON
082800090921     c*
082900091109     c                   ENDSR
083000090923     c*--------------------------------------------------------------------------------------------*
083100090923 ttt c* /TITLE Compongo il testo e spedisco una e-m@ail
083200090923     c*--------------------------------------------------------------------------------------------*
083300090923     c     sndeml        begsr
083400090923     c*
083500090923     c* Inizializzo variabili
083600090923     c                   movel     *blanks       wrkEml          253
083700090923     c                   movel     *blanks       wrkEmlMsg      5000
083800090923     c                   movel     *blanks       wrkEmlOgg        44
083900090923     c* Valorizzo i campi della e-m@ail
084000120306     c                   eval      wrkEml='cedalert@brt.it'
084100091109     c                   eval      wrkEmlOgg='CREA_TIVTA!!!'
084200090923     c                   EVAL      wrkEmlMsg = 'ERRORE IMPREVISTO IN '   +
084300090923     c                             'CREA_TIVTA'                          +':/N'+
084400090923     c                             ' '                                   +':/N'+
084500090923     c                             'Verificare dump del job: '           +':/N'+
084600091109     c                             %trim(JOB_NUM)+'/'+%trim(USER)+'/'          +
084700091109     c                             %trim(JOB_NAME)                       +':/N'+
084800091109     c                             ' '                                   +':/N'
084900090923     c*
085000090923     c                   call(e)   'TIS701C'
085100090923     c                   parm                    wrkEml
085200090923     c                   parm                    wrkEmlOgg
085300090923     c                   parm                    wrkEmlMsg
085400090923     c*
085500090923     c                   endsr
085600991102     c*--------------------------------------------------------------------------------------------*
085700090918 ttt c* operazioni iniziali
085800991102     c*--------------------------------------------------------------------------------------------*
085900991102     c     *inzsr        BEGSR
086000991102     c*
086100991102     c* ricevimento parametri
086200991104     c     *ENTRY        PLIST
086300991203     c                   PARM                    tis799dsI
086400091127 xxx c***                PARM                    RWRINEW
086500091127 xxx c***                PARM                    RDELNEW
086600091113     c*
086700091127 xxx c***                EVAL      RWRINEW = *zeros
086800091127 xxx c***                EVAL      RDELNEW = *zeros
086900091113     c*
087000010409     c* per prima cosa verifico i paramnetri ricevuti in input
087100091117     c                   IF        ksui99 <> *blanks AND
087200091117     c                             ksui99 <> *zeros
087300090918     c                   EVAL      flgbol = '0'                                 *elab. cliente unif.
087400010409     c                   ELSE
087500090918     c                   EVAL      flgbol = '1'                                 *elab. singola bolla
087600010409     c                   ENDIF
087700091207     c                   IF        clii99 <> *blanks AND
087800091207     c                             clii99 <> *zeros
087900091207     c                   EVAL      flgcli = '1'                                 *elab. cliente
088000091207     c                   ELSE
088100091207     c                   EVAL      flgcli = '0'                                 *elab. no x cliente
088200091207     c                   ENDIF
088300991102     c*
088400090918     c* chiavi di lettura
088500090918     c     Keytas_30     KLIST                                                  titas30c (unique)
088600090918     c                   KFLD                    aasi99                         -anno spedizione
088700090918     c                   KFLD                    lnpi99                         -linea partenza
088800090918     c                   KFLD                    nrsi99                         -serie
088900010409     c                   KFLD                    nspi99                         -spedizione
089000010409     c                   KFLD                    tbli99                         -tipo bolla
089100091014     c*
089200090918     c     Keyta4        KLIST                                                  tita430c/titaA30c
089300090918     c                   KFLD                    tasaas                         -anno spedizione
089400090918     c                   KFLD                    taslnp                         -linea partenza
089500090918     c                   KFLD                    tasnrs                         -serie
089600090918     c                   KFLD                    tasnsp                         -spedizione
089700090918     c                   KFLD                    ta4trc                         -tipo riferimento
089800091014     c*
089900090917     c     Keyvta_01     KLIST                                                  *tivta11l
090000090918     c                   KFLD                    ktadsp                          -data spedizione
090100090917     c                   KFLD                    lnpi99                          -linea partenza
090200090917     c                   KFLD                    nrsi99                          -serie
090300090917     c                   KFLD                    nspi99                          -spedizione
090400090917     c                   KFLD                    tbli99                          -tipo bolla
090500091014     c*
090600090918     c     Keyaco        KLIST                                                  *cnaco00f
090700090918     c                   KFLD                    acokut                          -utente
090800090918     c                   KFLD                    acokcc                          -capoconto
090900090918     c                   KFLD                    acoksc                          -sottoconto
091000091014     c*
091100091014     c     Keyvta_12     KLIST                                                  *tivta12l
091200091014     c                   KFLD                    vtaksu                          -cliente unificante
091300091014     c                   KFLD                    vtadsp                          -data spedizione
091400091014     c                   KFLD                    vtalnp                          -linea partenza
091500091014     c                   KFLD                    vtanrs                          -serie
091600091014     c                   KFLD                    vtansp                          -spedizione
091700091014     c                   KFLD                    vtatbl                          -tipo bolla
091800091116     c*
091900091116     c     Keyvta_13     KLIST                                                  *tivta12l
092000091116     c                   KFLD                    ksui99                          -cliente unificante
092100091116     c                   KFLD                    clii99                          -cliente KSC/CCM
092200991102     c*
092300991102     c* reperimento data corrente
092400091106     C                   EVAL      datcor = %DEC(%DATE() : *ISO)
092500991126     c*
092600120306     c* imposta la data corrente - 2 mesi (per clienti BRT)
092700991126     c     *ISO          MOVE      datcor        wdata
092800991209     c     wdata         SUBDUR    2:*M          wdata
092900991126     c     *ISO          MOVE      wdata         datcorpre
093000991102     c*
093100991102     c* caricamento tabelle occorrenti
093200090918     c                   EXSR      cartab
093300991026     c*
093400991026     c                   ENDSR
