000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200011017     H*         - RITORNO DATI AL CLIENTE  da sede
000300000000     H*--------------------------------------------------------------*
000400020726     FTABEL00F  IF   E           K DISK
000500020726     FAZORG01L  IF   E           K DISK
000600020726     FTNCSB10L  IF   E           K DISK
000700120405     FTNCSM03L  IF   E           K DISK
000800020726     FTITAS30C  IF   E           K DISK
000900131008     FYHBOS14L  IF   E           K DISK
001000020726     FTNVAS00T  UF A E             DISK    USROPN
001100050831     FTIVGD00F  UF A E             DISK    USROPN
001200001218     D*--------------------
001300001218     D* SCHIERA PER REPERIMENTO CLIENTI "FIGLI" DI UN UNIFICANTE
001400001218     D*--------------------
001500090407     D KUNI            S              7  0 DIM(1000) DESCEND
001600020726     D KNTW            S              3    DIM(1000)
001700001218     D*--------------------
001800020726     D* DS ESTERNE
001900001218     D*--------------------
002000020726     D DS4C          E DS
002100020726     D OG143         E DS
002200050831     D TNVASDS       E DS                  EXTNAME(TNVAS00T)
002300050909     D trul47ds      e ds
002400001218     D*--------------------
002500001218     D* DS DI RIDEFINIZIOINE PARAMETRI IN INPUT
002600001218     D*--------------------
002700050831     D DSINPUT         DS
002800050831     D  DATAPDA                       8  0
002900050831     D  DATAPA                        8  0
003000050831     D  DATASDA                       8  0
003100050831     D  DATASA                        8  0
003200050831     D  CODCLI                        7  0
003300050831     D  FLGUNI                        1
003400050831     D  FLGOPZ                        1
003500080917     D  CODCLIVAS                     7  0
003600100312     D  TIPFILE                       2
003700020726     D*--------------------
003800020726     D* VARIABILI DI WRK
003900020726     D*--------------------
004000020726     D  wrkDATA                       8  0 INZ(*zeros)
004100020726     D  CHKREC                        1    INZ(*blanks)
004200020726     D  wrkFIL                        3  0 INZ(*zeros)
004300020726     D*--------------------
004400001218
004500001218
004600001218
004700920812     C*---------------------------------------------------------------*
004800001218     C* MAIN
004900001218     C*---------------------------------------------------------------*
005000001218     C                   exsr      cartbl
005100001218     C                   exsr      chkpar
005200001218     C                   exsr      procedi
005300001218     C*
005400001218     C                   seton                                        LR
005500001218
005600001218
005700001218
005800001218     C*------------------------------------------------------------------------*
005900001218     C* CHKPAR - Routine di controllo parametri ricevuti in input
006000001218     C*------------------------------------------------------------------------*
006100001218     C     CHKPAR        BEGSR
006200001218     C*
006300001218     C* Verifico le date
006400050831     C                   if        DATAPA = *zeros
006500050831     C                   movel     *all'9'       DATAPA
006600001218     C                   endif
006700050831     C                   if        DATASA = *zeros
006800050831     C                   movel     *all'9'       DATASA
006900020726     C                   endif
007000050831     C*
007100050831     C* Verifico la modalità d scrittura output richiesta:
007200050831     C*   1 = crea membro "RIC" in file TNVAS00T
007300050831     C*   2 = scrivi record in file TIVGD00F
007400050831     C                   setoff                                       2526
007500050831     C                   if        FLGOPZ = '1'
007600050831     C                   seton                                        25
007700050831     C                   endif
007800050831     C                   if        FLGOPZ = '2'
007900050831     C                   seton                                        26
008000050831     C                   endif
008100001218     C*
008200001218     C                   ENDSR
008300001218     C*------------------------------------------------------------------------*
008400001218
008500001218
008600001218
008700001218     C*------------------------------------------------------------------------*
008800001218     C* PROCEDI - Routine principale
008900001218     C*------------------------------------------------------------------------*
009000001218     C     PROCEDI       BEGSR
009100050909     C*
009200050909     C* Inizializzo variabili d wrk
009300050909     C                   movel     'N'           wProcedi          1
009400050831     C*
009500050831     C* Apro il file di output
009600050909     C                   if        *in25 = *on
009700050909     C                   open      tnvas00t
009800050909     C                   movel     'S'           wProcedi
009900050909     C                   endif
010000050909     C*
010100050909     C                   if        *in26 = *on
010200050909     C                   open      tivgd00f
010300050909     C*
010400050909     C* Come prima cosa avvio il blocco elaborazione TIVGD x tipo file corrente: 'VS'
010500050909     C                   clear                   trul47ds
010600050909     C                   eval      d47opz  = 'I'
010700120927     C                   eval      d47tip  = TIPFILE
010800050909     C                   eval      d47lck  = 'N'
010900050909     C                   eval      d47chkj = 'N'
011000050909     C                   eval      d47pgm  = 'TIS7VSR'
011100050909     C                   call      'TRUL47R'
011200050909     C                   parm                    trul47ds
011300050909     C*
011400050909     C* Se elaborazione consentita => proseguo
011500120928     C***                if        d47sts <> 'A'
011600050909     C                   movel     'S'           wProcedi
011700120928     C***                endif
011800050909     C                   endif
011900050909     C*
012000050909     C* Se ok a procedere => elaboro
012100050909     C                   if        wProcedi = 'S'
012200001218     C*
012300001218     C* Ciclo sulla schiera dei clienti figli dell'unificante preso in input
012400001218     C                   z-add     1             I
012500090407     C                   sorta     KUNI
012600001218     C                   dow       KUNI(I) > *zeros
012700001218     C*
012800020726     C* Mi posiziono a inizio file contrassegni (TNCSB10L)
012900020726     C                   eval      csbSTA = *zeros
013000020726     C                   eval      csbCDI = KUNI(I)
013100060110     C                   eval      csbDDP = DATAPDA
013200060110     C                   movel     *blanks       CHKREC
013300060110     C     KEYCSB2       setll     tncsb10l
013400020726     C     KEYCSB1       reade     tncsb10l
013500020726     C                   dow       not %eof(tncsb10l) and
013600020726     C                             CHKREC <> 'X'
013700001218     C                   exsr      verifica
013800001218     C                   if        CHKREC = 'S'
013900020726     C                   exsr      scriviVAS
014000001218     C                   endif
014100020726     C     KEYCSB1       reade     tncsb10l
014200001218     C                   enddo
014300001218     C*
014400001218     C                   add       1             I
014500001218     C                   enddo
014600050909     C*
014700050909     C                   endif
014800050831     C*
014900050831     C* Chiudo il file di output
015000050831     C   25              close     tnvas00t
015100050831     C   26              close     tivgd00f
015200050909     C*
015300050909     C* Infine elimino il blocco elaborazione TIVGD x tipo file corrente: 'VS'
015400050909     C                   clear                   trul47ds
015500050909     C                   eval      d47opz  = 'F'
015600120928     C                   eval      d47tip  = TIPFILE
015700050909     C                   call      'TRUL47R'
015800050909     C                   parm                    trul47ds
015900001218     C*
016000001218     C                   ENDSR
016100001218     C*------------------------------------------------------------------------*
016200001218
016300001218
016400001218
016500001218     C*------------------------------------------------------------------------*
016600020726     C* VERIFICA - Routine di verifica validità record corrente
016700001218     C*------------------------------------------------------------------------*
016800001218     C     VERIFICA      BEGSR
016900030210     C*
017000030210     C                   movel     'S'           CHKREC
017100001218     C*
017200001218     C* Verifico le date
017300001218     C                   if        CHKREC = 'S'
017400050831     C                   if        csbDDP < DATAPDA or
017500050831     C                             csbDDP > DATAPA
017600020726     C                   movel     'X'           CHKREC
017700001218     C                   endif
017800001218     C                   endif
017900020726     C                   if        CHKREC = 'S'
018000020726     C                   eval      wrkDATA = csbAAS*10000 + csbMGS
018100050831     C                   if        wrkDATA < DATASDA or
018200050831     C                             wrkDATA > DATASA
018300020726     C                   movel     'N'           CHKREC
018400020726     C                   endif
018500020726     C                   endif
018600001218     C*
018700001218     C* Verifico il codice cliente
018800001218     C                   if        CHKREC = 'S'
018900020726     C                   if        csbCDI <> KUNI(I)
019000001218     C                   movel     'N'           CHKREC
019100001218     C                   endif
019200001218     C                   endif
019300001218     C*
019400020726     C* Verifico che il contrassegno sia stato pagato
019500001218     C                   if        CHKREC = 'S'
019600020726     C                   if        csbDDP > *zeros
019700001218     C                   else
019800001218     C                   movel     'N'           CHKREC
019900001218     C                   endif
020000001218     C                   endif
020100001218     C*
020200001218     C                   ENDSR
020300001218     C*------------------------------------------------------------------------*
020400001218
020500120405
020600120405
020700001218
020800001218     C*------------------------------------------------------------------------*
020900020726     C* SCRIVIVAS - Routine di scrittura file esiti di consegna (TNVAS)
021000001218     C*------------------------------------------------------------------------*
021100020726     C     SCRIVIVAS     BEGSR
021200001218     C*
021300020726     C                   CLEAR                   tnvas000
021400001218     C*
021500020726     C                   movel     csbLNP        VASLNP
021600020726     C                   movel     csbAAS        VASAAS
021700020726     C                   movel     csbNRS        VASNRS
021800020726     C                   movel     csbNSP        VASNSP
021900020726     C                   movel     csbLNA        VASLNA
022000020726     C                   movel     csbRMN        VASRMN
022100020726     C                   movel     csbRMA        VASRMA
022200020726     C                   z-add     csbCAS        VASCAS
022300020726     C                   movel     csbVCA        VASVCA
022400020726     C                   movel     csbNRA        VASNRA
022500020726     C                   z-add     csbDTE        VASDTE
022600020726     C                   movel     csbTPI        VASTPI
022700020726     C                   z-add     csbABI        VASABI
022800020726     C                   z-add     csbCAB        VASCAI
022900020726     C                   movel     csbNDP        VASNDP
023000020726     C                   movel     csbDDP        VASDDP
023100020726     C                   movel     csbFPC        VASFPC
023200020726     C                   movel     csbCDI        VASKSC
023300020726     C                   movel     *BLANK        VASCCB
023400020726     C* Se il cliente è poste metto il numero segnacollo in VASDTE
023500020726     C* e azzero i campi degli incassi
023600020726     C                   if        KNTW(I) = 'PPT'
023700020726     C                   clear                   VASDTE
023800020726     C                   clear                   VASNRA
023900020726     C                   clear                   VASTPI
024000020726     C                   clear                   VASABI
024100020726     C                   clear                   VASCAI
024200020726     C     KEYTAS        chain     titas30c
024300020726     C                   if        %found(titas30c)
024400020726     C                   z-add     tasNCD        VASDTE
024500020726     C                   endif
024600020726     C                   endif
024700131008     C*
024800131008     C* Aggancio l'archivio dei pagamenti automatici
024900131008     C                   eval      bonELA     = *blanks
025000131008     C                   eval      bonSOCIETA = '201'
025100131008     C                   eval      bonKCC     = '000465'
025200131009     C                   eval      bonCDI     = '0'+%trim(%editc(csbCDI:'X'))
025300131008     C     KEYBOS        chain     yhbos14l
025400131008     C                   if        %found(yhbos14l)
025500131008     C                   eval      VASDVA = %dec(bonDVF:*ISO)
025600131008     C                   endif
025700120405     C*
025800120405     C* Verifico incasso contrassegno con assegni mittente multipli
025900120405     C                   if        csbTPI               = 'M'    AND
026000120405     C                             %subst(csbNRA:10:1) <> ' '    AND
026100120405     C                             csbABI               = *zeros AND
026200120405     C                             csbCAB               = *zeros
026300120405     C                   exsr      LSTASS
026400120405     C                   endif
026500001218     C*
026600050831     C   25              WRITE     TNVAS000
026700050901     C   26              exsr      WRIVGD
026800001218     C*
026900001218     C                   ENDSR
027000020726     C*------------------------------------------------------------------------*
027100120405
027200120405
027300120405
027400120405
027500120405     C*---------------------------------------------------------------*
027600120405     C*  LSTASS - Reperimento lista assegni incassati x C/A
027700120405     C*---------------------------------------------------------------*
027800120405     C     LSTASS        BEGSR
027900120405     C*
028000120405     C     csbNRA        chain     tncsm03l
028100120405     C                   if        %found(tncsm03l)
028200120405     C                   movel     csmNRA        VASNRA
028300120405     C                   z-add     csmABI        VASABI
028400120405     C                   z-add     csmCAB        VASCAI
028500120405     C                   endif
028600120405     C*
028700120405     C                   ENDSR
028800120405     C*------------------------------------------------------------------------*
028900050831
029000050831
029100050831
029200050831
029300050831     C*------------------------------------------------------------------------*
029400050901     C* WRIVGD - Routine di scrittura file TIVGD (file VAS generico download)
029500050831     C*------------------------------------------------------------------------*
029600050901     C     WRIVGD        BEGSR
029700050831     C*
029800050831     C                   clear                   tivgd000
029900080407     C                   eval      vgdDTA = %subst(TNVASDS:1:%size(TNVASDS))
030000100312     C                   eval      vgdTIP = TIPFILE
030100050831     C                   movel     *all'0'       vgdKSU
030200080917     C                   move      CODCLIVAS     vgdKSU
030300050831     C                   eval      vgdTSC = 'WW'
030400050831     C                   eval      vgdDAT = datcor
030500050831     C                   eval      vgdPGM = 'TIS7VSR'
030600050831     C                   write     tivgd000
030700050831     C*
030800050831     C                   ENDSR
030900050831     C*------------------------------------------------------------------------*
031000020726
031100020726
031200020726
031300001218
031400001218     C*------------------------------------------------------------------------*
031500001218     C* CARTBL - Routine di caricamento dati tabellati
031600001218     C*------------------------------------------------------------------------*
031700001218     C     CARTBL        BEGSR
031800001218     C*
031900001218     C                   Z-ADD     0             I                 4 0
032000050831     C                   IF        FLGUNI = 'S'
032100010301     C                   ADD       1             I
032200011017     C                   MOVEL     CODCLI        KUNI(I)
032300020726     C                   EXSR      REPNTW
032400010301     C                   ELSE
032500020726     C                   MOVEL     '4C'          COD
032600001218     C     KTAB          CHAIN     TABEL                              31
032700001218     C     *IN31         DOWEQ     '0'
032800001218     C     TBLFLG        IFEQ      ' '
032900020726     C                   MOVEL     TBLUNI        DS4C
033000090427     C                   IF        (§4CCKS = CODCLI AND §4CTSD <> *BLANKS)
033100001218     C                   ADD       1             I
033200001218     C                   MOVEL     TBLKEY        KUNI(I)
033300020726     C                   EXSR      REPNTW
033400001218     C                   ENDIF
033500001218     C                   ENDIF
033600001218     C     KTAB          READE     TABEL                                  31
033700001218     C                   ENDDO
033800010301     C                   ENDIF
033900001218     C*
034000001218     C                   ENDSR
034100001218     C*---------------------------------------------------------------*
034200020726
034300020726
034400020726
034500020726
034600020726     C*------------------------------------------------------------------------*
034700020726     C* REPNTW - Routine di reperimento network cliente
034800020726     C*------------------------------------------------------------------------*
034900020726     C     REPNTW        BEGSR
035000020726     C*
035100020726     C                   MOVEL(P)  KUNI(I)       wrkFIL
035200020726     C     wrkFIL        CHAIN     AZORG01L
035300020726     C                   IF        %found(AZORG01L)
035400020726     C                   MOVEL     ORGDE3        OG143
035500020726     C                   MOVEL     §OGNTW        KNTW(I)
035600020726     C                   ENDIF
035700020726     C*
035800020726     C                   ENDSR
035900020726     C*---------------------------------------------------------------*
036000001218
036100001218
036200001218
036300001218
036400001218     C*------------------------------------------------------------------------*
036500001218     C* *INZSR - ROUTINE INIZIALE
036600001218     C*------------------------------------------------------------------------*
036700001218     C     *INZSR        BEGSR
036800001218     C*
036900001218     C     *ENTRY        PLIST
037000100312     C                   PARM                    PARAM            50
037100001218     C*
037200001218     C                   MOVEL     PARAM         DSINPUT
037300001218     C                   Z-ADD     1             CODUT
037400050831     C*
037500050831     C* Determino la data corrente
037600100312     C                   Z-ADD     *zeros        DATCOR            8 0
037700100312     C                   EVAL      DATCOR = %dec(%date() : *ISO)
037800001218     C*
037900001218     C* Definizioni chiavi
038000001218     C*
038100020726     C     KEYCSB1       KLIST
038200020726     C                   KFLD                    csbSTA
038300020726     C                   KFLD                    csbCDI
038400020726     C*
038500020726     C     KEYCSB2       KLIST
038600020726     C                   KFLD                    csbSTA
038700020726     C                   KFLD                    csbCDI
038800020726     C                   KFLD                    csbDDP
038900020726     C*
039000020726     C     KEYTAS        KLIST
039100020726     C                   KFLD                    csbAAS
039200020726     C                   KFLD                    csbLNP
039300020726     C                   KFLD                    csbNRS
039400020726     C                   KFLD                    csbNSP
039500020726     C                   KFLD                    csbTBL
039600001218     C*
039700001218     C     KTAB          KLIST
039800001218     C                   KFLD                    CODUT             1 0
039900001218     C                   KFLD                    COD               2
040000131008     C*
040100131008     C     KEYBOS        KLIST
040200131008     C                   KFLD                    bonELA
040300131008     C                   KFLD                    bonSOCIETA
040400131008     C                   KFLD                    bonKCC
040500131008     C                   KFLD                    bonCDI
040600131008     C                   KFLD                    csbLNP
040700131008     C                   KFLD                    csbAAS
040800131008     C                   KFLD                    csbNRS
040900131008     C                   KFLD                    csbNSP
041000001218     C*
041100001218     C                   ENDSR
