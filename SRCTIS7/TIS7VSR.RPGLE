000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200170227     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP(*CALLER)
000300011017     H*         - RITORNO DATI AL CLIENTE  da sede
000400000000     H*--------------------------------------------------------------*
000500020726     FTABEL00F  IF   E           K DISK
000600020726     FAZORG01L  IF   E           K DISK
000700020726     FTNCSB10L  IF   E           K DISK
000800120405     FTNCSM03L  IF   E           K DISK
000900020726     FTITAS30C  IF   E           K DISK
001000131008     FYHBOS14L  IF   E           K DISK
001100170227     FTNVAS00T  UF A E             DISK    USROPN COMMIT
001200170227     FTIVGD00F  UF A E             DISK    USROPN COMMIT
001300001218     D*--------------------
001400001218     D* SCHIERA PER REPERIMENTO CLIENTI "FIGLI" DI UN UNIFICANTE
001500001218     D*--------------------
001600090407     D KUNI            S              7  0 DIM(1000) DESCEND
001700020726     D KNTW            S              3    DIM(1000)
001800001218     D*--------------------
001900020726     D* DS ESTERNE
002000001218     D*--------------------
002100170227     D psds           sds
002200170227     D  procname         *PROC
002300020726     D DS4C          E DS
002400020726     D OG143         E DS
002500050831     D TNVASDS       E DS                  EXTNAME(TNVAS00T)
002600170227     D TIS7VASDS     E DS
002700001218     D*--------------------
002800001218     D* DS DI RIDEFINIZIOINE PARAMETRI IN INPUT
002900001218     D*--------------------
003000050831     D DSINPUT         DS
003100050831     D  DATAPDA                       8  0
003200050831     D  DATAPA                        8  0
003300050831     D  DATASDA                       8  0
003400050831     D  DATASA                        8  0
003500050831     D  CODCLI                        7  0
003600050831     D  FLGUNI                        1
003700050831     D  FLGOPZ                        1
003800080917     D  CODCLIVAS                     7  0
003900100312     D  TIPFILE                       2
004000020726     D*--------------------
004100020726     D* VARIABILI DI WRK
004200020726     D*--------------------
004300020726     D  wrkDATA                       8  0 INZ(*zeros)
004400020726     D  CHKREC                        1    INZ(*blanks)
004500020726     D  wrkFIL                        3  0 INZ(*zeros)
004600020726     D*--------------------
004700001218
004800001218
004900001218
005000920812     C*---------------------------------------------------------------*
005100001218     C* MAIN
005200001218     C*---------------------------------------------------------------*
005300170227     C*
005400170227     C* Avvio il monitoring del processo
005500170227     C                   monitor
005600170227     C*
005700001218     C                   exsr      cartbl
005800001218     C                   exsr      chkpar
005900001218     C                   exsr      procedi
006000170227     C*
006100170227     C* Sabcisco il commit
006200170309     C                   commit(e)
006300170227     C*
006400170227     C* Su errore
006500170227     C                   on-error
006600170227     C                   exsr      exeerr
006700170227     C*
006800170227     C* Fine monitoring
006900170227     C                   endmon
007000001218     C*
007100001218     C                   seton                                        LR
007200001218
007300001218
007400001218
007500001218     C*------------------------------------------------------------------------*
007600001218     C* CHKPAR - Routine di controllo parametri ricevuti in input
007700001218     C*------------------------------------------------------------------------*
007800001218     C     CHKPAR        BEGSR
007900001218     C*
008000001218     C* Verifico le date
008100050831     C                   if        DATAPA = *zeros
008200050831     C                   movel     *all'9'       DATAPA
008300001218     C                   endif
008400050831     C                   if        DATASA = *zeros
008500050831     C                   movel     *all'9'       DATASA
008600020726     C                   endif
008700050831     C*
008800050831     C* Verifico la modalità d scrittura output richiesta:
008900050831     C*   1 = crea membro "RIC" in file TNVAS00T
009000050831     C*   2 = scrivi record in file TIVGD00F
009100050831     C                   setoff                                       2526
009200050831     C                   if        FLGOPZ = '1'
009300050831     C                   seton                                        25
009400050831     C                   endif
009500050831     C                   if        FLGOPZ = '2'
009600050831     C                   seton                                        26
009700050831     C                   endif
009800001218     C*
009900001218     C                   ENDSR
010000001218     C*------------------------------------------------------------------------*
010100001218
010200001218
010300001218
010400001218     C*------------------------------------------------------------------------*
010500001218     C* PROCEDI - Routine principale
010600001218     C*------------------------------------------------------------------------*
010700001218     C     PROCEDI       BEGSR
010800050909     C*
010900050909     C* Inizializzo variabili d wrk
011000050909     C                   movel     'N'           wProcedi          1
011100050831     C*
011200050831     C* Apro il file di output
011300170227     C                   if        *in25
011400050909     C                   open      tnvas00t
011500050909     C                   movel     'S'           wProcedi
011600050909     C                   endif
011700050909     C*
011800170227     C                   if        *in26
011900050909     C                   open      tivgd00f
012000170227     C*
012100170227     C* Stacco progressivo univoco download
012200170227     C                   CLEAR                   TIS7VASDS
012300170227     C                   EVAL      i§7VASOPZ = 'PRG'
012400170227     C                   CALL(e)   'TIS7VASR1'
012500170227     C                   PARM                    TIS7VASDS
012600170227     C*
012700170227     C* Se OK => proseguo
012800170227     C                   if        not %error AND
012900170227     C                             o§7VASOK = *on AND o§7VASPRG <> *blanks
013000170227     C                   movel     'S'           wProcedi
013100170227     C                   endif
013200050909     C                   endif
013300050909     C*
013400050909     C* Se ok a procedere => elaboro
013500050909     C                   if        wProcedi = 'S'
013600001218     C*
013700001218     C* Ciclo sulla schiera dei clienti figli dell'unificante preso in input
013800001218     C                   z-add     1             I
013900090407     C                   sorta     KUNI
014000001218     C                   dow       KUNI(I) > *zeros
014100001218     C*
014200020726     C* Mi posiziono a inizio file contrassegni (TNCSB10L)
014300020726     C                   eval      csbSTA = *zeros
014400020726     C                   eval      csbCDI = KUNI(I)
014500060110     C                   eval      csbDDP = DATAPDA
014600060110     C                   movel     *blanks       CHKREC
014700060110     C     KEYCSB2       setll     tncsb10l
014800020726     C     KEYCSB1       reade     tncsb10l
014900020726     C                   dow       not %eof(tncsb10l) and
015000020726     C                             CHKREC <> 'X'
015100001218     C                   exsr      verifica
015200001218     C                   if        CHKREC = 'S'
015300020726     C                   exsr      scriviVAS
015400001218     C                   endif
015500020726     C     KEYCSB1       reade     tncsb10l
015600001218     C                   enddo
015700001218     C*
015800001218     C                   add       1             I
015900001218     C                   enddo
016000050909     C*
016100050909     C                   endif
016200050831     C*
016300050831     C* Chiudo il file di output
016400050831     C   25              close     tnvas00t
016500050831     C   26              close     tivgd00f
016600170227     C*
016700170227     C* Finalizzo la transazione
016800170227     C                   if        *in26
016900170227     C                   EVAL      i§7VASOPZ  = 'RLS'
017000170227     C                   EVAL      i§7VASTIP  = TIPFILE
017100170227     C                   EVAL      i§7VASKSU  = '0'+%editc(CODCLIVAS:'X')
017200170227     C                   EVAL      i§7VASTSC  = 'WW'
017300170227     C                   EVAL      i§7VASSTO  = '?'
017400170227     C                   EVAL      i§7VASSTTO = 'L'
017500170227     C                   EVAL      i§7VASPRG  = o§7VASPRG
017600170227     C                   CALL(e)   'TIS7VASR1'
017700170227     C                   PARM                    TIS7VASDS
017800170227     C*
017900170227     C* Verifico esito chiamata
018000170227     C                   if        %error OR o§7VASOK = *off
018100170227     C                   exsr      EXEERR
018200170227     C                   endif
018300170227     C                   endif
018400001218     C*
018500001218     C                   ENDSR
018600001218     C*------------------------------------------------------------------------*
018700001218
018800001218
018900001218
019000001218     C*------------------------------------------------------------------------*
019100020726     C* VERIFICA - Routine di verifica validità record corrente
019200001218     C*------------------------------------------------------------------------*
019300001218     C     VERIFICA      BEGSR
019400030210     C*
019500030210     C                   movel     'S'           CHKREC
019600001218     C*
019700001218     C* Verifico le date
019800001218     C                   if        CHKREC = 'S'
019900050831     C                   if        csbDDP < DATAPDA or
020000050831     C                             csbDDP > DATAPA
020100020726     C                   movel     'X'           CHKREC
020200001218     C                   endif
020300001218     C                   endif
020400020726     C                   if        CHKREC = 'S'
020500020726     C                   eval      wrkDATA = csbAAS*10000 + csbMGS
020600050831     C                   if        wrkDATA < DATASDA or
020700050831     C                             wrkDATA > DATASA
020800020726     C                   movel     'N'           CHKREC
020900020726     C                   endif
021000020726     C                   endif
021100001218     C*
021200001218     C* Verifico il codice cliente
021300001218     C                   if        CHKREC = 'S'
021400020726     C                   if        csbCDI <> KUNI(I)
021500001218     C                   movel     'N'           CHKREC
021600001218     C                   endif
021700001218     C                   endif
021800001218     C*
021900020726     C* Verifico che il contrassegno sia stato pagato
022000001218     C                   if        CHKREC = 'S'
022100020726     C                   if        csbDDP > *zeros
022200001218     C                   else
022300001218     C                   movel     'N'           CHKREC
022400001218     C                   endif
022500001218     C                   endif
022600001218     C*
022700001218     C                   ENDSR
022800001218     C*------------------------------------------------------------------------*
022900001218
023000120405
023100120405
023200001218
023300001218     C*------------------------------------------------------------------------*
023400020726     C* SCRIVIVAS - Routine di scrittura file esiti di consegna (TNVAS)
023500001218     C*------------------------------------------------------------------------*
023600020726     C     SCRIVIVAS     BEGSR
023700001218     C*
023800020726     C                   CLEAR                   tnvas000
023900001218     C*
024000020726     C                   movel     csbLNP        VASLNP
024100020726     C                   movel     csbAAS        VASAAS
024200020726     C                   movel     csbNRS        VASNRS
024300020726     C                   movel     csbNSP        VASNSP
024400020726     C                   movel     csbLNA        VASLNA
024500020726     C                   movel     csbRMN        VASRMN
024600020726     C                   movel     csbRMA        VASRMA
024700020726     C                   z-add     csbCAS        VASCAS
024800020726     C                   movel     csbVCA        VASVCA
024900020726     C                   movel     csbNRA        VASNRA
025000020726     C                   z-add     csbDTE        VASDTE
025100020726     C                   movel     csbTPI        VASTPI
025200020726     C                   z-add     csbABI        VASABI
025300020726     C                   z-add     csbCAB        VASCAI
025400020726     C                   movel     csbNDP        VASNDP
025500020726     C                   movel     csbDDP        VASDDP
025600020726     C                   movel     csbFPC        VASFPC
025700020726     C                   movel     csbCDI        VASKSC
025800020726     C                   movel     *BLANK        VASCCB
025900020726     C* Se il cliente è poste metto il numero segnacollo in VASDTE
026000020726     C* e azzero i campi degli incassi
026100020726     C                   if        KNTW(I) = 'PPT'
026200020726     C                   clear                   VASDTE
026300020726     C                   clear                   VASNRA
026400020726     C                   clear                   VASTPI
026500020726     C                   clear                   VASABI
026600020726     C                   clear                   VASCAI
026700020726     C     KEYTAS        chain     titas30c
026800020726     C                   if        %found(titas30c)
026900020726     C                   z-add     tasNCD        VASDTE
027000020726     C                   endif
027100020726     C                   endif
027200131008     C*
027300131008     C* Aggancio l'archivio dei pagamenti automatici
027400131008     C                   eval      bonELA     = *blanks
027500131008     C                   eval      bonSOCIETA = '201'
027600131008     C                   eval      bonKCC     = '000465'
027700131009     C                   eval      bonCDI     = '0'+%trim(%editc(csbCDI:'X'))
027800131008     C     KEYBOS        chain     yhbos14l
027900131008     C                   if        %found(yhbos14l)
028000131008     C                   eval      VASDVA = %dec(bonDVF:*ISO)
028100131008     C                   endif
028200120405     C*
028300120405     C* Verifico incasso contrassegno con assegni mittente multipli
028400120405     C                   if        csbTPI               = 'M'    AND
028500120405     C                             %subst(csbNRA:10:1) <> ' '    AND
028600120405     C                             csbABI               = *zeros AND
028700120405     C                             csbCAB               = *zeros
028800120405     C                   exsr      LSTASS
028900120405     C                   endif
029000001218     C*
029100050831     C   25              WRITE     TNVAS000
029200050901     C   26              exsr      WRIVGD
029300001218     C*
029400001218     C                   ENDSR
029500020726     C*------------------------------------------------------------------------*
029600120405
029700120405
029800120405
029900120405
030000120405     C*---------------------------------------------------------------*
030100120405     C*  LSTASS - Reperimento lista assegni incassati x C/A
030200120405     C*---------------------------------------------------------------*
030300120405     C     LSTASS        BEGSR
030400120405     C*
030500120405     C     csbNRA        chain     tncsm03l
030600120405     C                   if        %found(tncsm03l)
030700120405     C                   movel     csmNRA        VASNRA
030800120405     C                   z-add     csmABI        VASABI
030900120405     C                   z-add     csmCAB        VASCAI
031000120405     C                   endif
031100120405     C*
031200120405     C                   ENDSR
031300120405     C*------------------------------------------------------------------------*
031400050831
031500050831
031600050831
031700050831
031800050831     C*------------------------------------------------------------------------*
031900050901     C* WRIVGD - Routine di scrittura file TIVGD (file VAS generico download)
032000050831     C*------------------------------------------------------------------------*
032100050901     C     WRIVGD        BEGSR
032200050831     C*
032300050831     C                   clear                   tivgd000
032400080407     C                   eval      vgdDTA = %subst(TNVASDS:1:%size(TNVASDS))
032500100312     C                   eval      vgdTIP = TIPFILE
032600170227     C                   eval      vgdKSU = '0'+%editc(CODCLIVAS:'X')
032700050831     C                   eval      vgdTSC = 'WW'
032800050831     C                   eval      vgdDAT = datcor
032900170227     C                   eval      vgdPRG = o§7VASPRG
033000170227     C                   eval      vgdPGM = procname
033100170227     C                   eval      vgdSTO = '?'
033200050831     C                   write     tivgd000
033300050831     C*
033400050831     C                   ENDSR
033500050831     C*------------------------------------------------------------------------*
033600020726
033700020726
033800020726
033900001218
034000001218     C*------------------------------------------------------------------------*
034100001218     C* CARTBL - Routine di caricamento dati tabellati
034200001218     C*------------------------------------------------------------------------*
034300001218     C     CARTBL        BEGSR
034400001218     C*
034500001218     C                   Z-ADD     0             I                 4 0
034600050831     C                   IF        FLGUNI = 'S'
034700010301     C                   ADD       1             I
034800011017     C                   MOVEL     CODCLI        KUNI(I)
034900020726     C                   EXSR      REPNTW
035000010301     C                   ELSE
035100020726     C                   MOVEL     '4C'          COD
035200001218     C     KTAB          CHAIN     TABEL                              31
035300001218     C     *IN31         DOWEQ     '0'
035400001218     C     TBLFLG        IFEQ      ' '
035500020726     C                   MOVEL     TBLUNI        DS4C
035600090427     C                   IF        (§4CCKS = CODCLI AND §4CTSD <> *BLANKS)
035700001218     C                   ADD       1             I
035800001218     C                   MOVEL     TBLKEY        KUNI(I)
035900020726     C                   EXSR      REPNTW
036000001218     C                   ENDIF
036100001218     C                   ENDIF
036200001218     C     KTAB          READE     TABEL                                  31
036300001218     C                   ENDDO
036400010301     C                   ENDIF
036500001218     C*
036600001218     C                   ENDSR
036700001218     C*---------------------------------------------------------------*
036800020726
036900020726
037000020726
037100020726
037200020726     C*------------------------------------------------------------------------*
037300020726     C* REPNTW - Routine di reperimento network cliente
037400020726     C*------------------------------------------------------------------------*
037500020726     C     REPNTW        BEGSR
037600020726     C*
037700020726     C                   MOVEL(P)  KUNI(I)       wrkFIL
037800020726     C     wrkFIL        CHAIN     AZORG01L
037900020726     C                   IF        %found(AZORG01L)
038000020726     C                   MOVEL     ORGDE3        OG143
038100020726     C                   MOVEL     §OGNTW        KNTW(I)
038200020726     C                   ENDIF
038300020726     C*
038400020726     C                   ENDSR
038500020726     C*---------------------------------------------------------------*
038600170227
038700170227
038800170227
038900170227     C*------------------------------------------------------------------------*
039000170227     C* EXEERR - Routine di esecuzione azioni su Errore
039100170227     C*------------------------------------------------------------------------*
039200170227     C     EXEERR        BEGSR
039300170227     C*
039400170227     C                   dump(A)
039500170322     C                   rolbk(e)
039600170227     C                   seton                                        lr
039700170227     C                   return
039800170227     C*
039900170227     C                   ENDSR
040000170227     C*------------------------------------------------------------------------*
040100001218
040200001218
040300001218
040400001218     C*------------------------------------------------------------------------*
040500001218     C* *INZSR - ROUTINE INIZIALE
040600001218     C*------------------------------------------------------------------------*
040700001218     C     *INZSR        BEGSR
040800001218     C*
040900001218     C     *ENTRY        PLIST
041000100312     C                   PARM                    PARAM            50
041100001218     C*
041200001218     C                   MOVEL     PARAM         DSINPUT
041300001218     C                   Z-ADD     1             CODUT
041400050831     C*
041500050831     C* Determino la data corrente
041600100312     C                   Z-ADD     *zeros        DATCOR            8 0
041700100312     C                   EVAL      DATCOR = %dec(%date() : *ISO)
041800001218     C*
041900001218     C* Definizioni chiavi
042000001218     C*
042100020726     C     KEYCSB1       KLIST
042200020726     C                   KFLD                    csbSTA
042300020726     C                   KFLD                    csbCDI
042400020726     C*
042500020726     C     KEYCSB2       KLIST
042600020726     C                   KFLD                    csbSTA
042700020726     C                   KFLD                    csbCDI
042800020726     C                   KFLD                    csbDDP
042900020726     C*
043000020726     C     KEYTAS        KLIST
043100020726     C                   KFLD                    csbAAS
043200020726     C                   KFLD                    csbLNP
043300020726     C                   KFLD                    csbNRS
043400020726     C                   KFLD                    csbNSP
043500020726     C                   KFLD                    csbTBL
043600001218     C*
043700001218     C     KTAB          KLIST
043800001218     C                   KFLD                    CODUT             1 0
043900001218     C                   KFLD                    COD               2
044000131008     C*
044100131008     C     KEYBOS        KLIST
044200131008     C                   KFLD                    bonELA
044300131008     C                   KFLD                    bonSOCIETA
044400131008     C                   KFLD                    bonKCC
044500131008     C                   KFLD                    bonCDI
044600131008     C                   KFLD                    csbLNP
044700131008     C                   KFLD                    csbAAS
044800131008     C                   KFLD                    csbNRS
044900131008     C                   KFLD                    csbNSP
045000001218     C*
045100001218     C                   ENDSR
