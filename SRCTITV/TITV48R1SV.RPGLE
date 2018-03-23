000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200991027     H dftactgrp(*yes)
000300991027
000400030429     FTNVAS00T  uF   E             DISK
000900030409     FTELE2XLS5 O    E             DISK
001000030429     FTNVAS00S  O    E             DISK    rename(tnvas000:tnvassss)
001200030409     FTITAS30C  IF   E           K DISK
001400030409     FTNCSB03L  IF   E           K DISK
001500030409     FTIVIR01L  UF   E           K DISK
001700991027
001800030429     D prmppt          s             50
001900991027     D prmesito        s              1
002000991027     D wrkesito        s                   like(prmesito)
002500030409     D flgIntXLS5      s              1    inz('0')
003100030417     D wParmGgSet      s              1  0 inz(*zeros)
003200030325
003500030429     D dsvas         e ds                  extname(tnvas00t)
003800030409     D X16GGSETDS    e ds
004200030409
004300030409     D*---------------------
004400030409     D* DS "XSRDA8" - CONTROLLA DATA (8)
004500030409     D*---------------------
004600030409     D WLBDA8          DS
004700030409     D  G08DAT                 1      8  0
004800030409     D  G08INV                 9     16  0
004900030409     D  G08ERR                17     17
005000030409     D  G08TGI                18     22  0
005100030409
005200030409
005300030417     C*
005400030417     C* Verifico se ricevuti dei parametri (nella fatispecie il giorno della settimana in cui
005500030417     C* effettuare la traduzione
005600030417     C                   EVAL      wParmGgSet = 2                               * DFT = Lunedì
005700030417     C                   IF        %subst(prmppt:9:1) <> *blanks
005800030417     C                   EVAL      PiStr=%trim(%subst(prmppt:9:1))
005900030417     C                   EXSR      CHKNUM
006000030417     C                   IF        PiInt=*on
006100030417     C                   Z-ADD     PiVal         wParmGgSet
006200030417     C                   ENDIF
006300030417     C                   ENDIF
006400030409     C*
006500030409     C* Innanzitutto verifico se la traduzione è da eseguire in quanto il cliente vuole i dati
006600030409     C* solamente 1 volta a settimana (nella fattispecie abbiamo stabilito il lunedì)
006700030409     C                   CLEAR                   X16GGSETDS
006800030409     C     *ISO          MOVE      DATCOR        X16DATA
006900030409     C                   CALL      'X16GGSET'
007000030409     C                   PARM                    X16GGSETDS
007100030417     C                   IF        X16GIORNO = wParmGgSet                          * Giorno settim.
007200030409     C                   EVAL      virKSC = '00052454'
007300030429 xxx C                   EVAL      virSEQ = 006                                    * FONDAMENTALE *
007400030409     C     KEYvir01      CHAIN     TIVIR01L
007500030409     C                   IF        %found(TIVIR01L)
007600030417     C                   IF        %subst(virPPT:1:8) <>
007700030417     C                             %editw(DATCOR:'        ')
007800030409     C                   EXSR      traduci
007900030417     C                   EVAL      %subst(virPPT:1:8) =
008000030417     C                             %editw(DATCOR:'        ')
008100030409     C                   UPDATE    TIVIR000
008300030409     C                   ENDIF
008400030409     C                   ENDIF
008500030409     C                   ENDIF
008600030409     C*
008700030409     C                   EVAL      wrkesito = '0'
008800030409     C*
008900921023     C                   SETON                                        LR
009000030409
009100030409
009200030409
009300991027
009400991027     C     traduci       BEGSR
009500991027     C*
009600030429     C                   READ      TNVAS00T
009700030429     C                   DOW       not %eof(TNVAS00T)
009800991027     C*
009900030325     C* Verifico che nn vi siano nei campi alfa caratteri <"> che altrimenti sostituisco con <'>
010000030429     C     '"':''''      XLATE     dsvas         dsvas
010001030429     C*
010002030429     C                   EXSR      TRADXLS5
016300030429     C                   WRITE     TNVASsss
016400030429     C                   DELETE    TNVAS000
016700991027     C*
016800030429     C                   READ      TNVAS00T
016900030325     C                   ENDDO
017000991027     C*
017100910830     C                   ENDSR
036900030409
037000030409
037100030409
037200030429     C     TRADXLS5      BEGSR
037300030409     C*
037400030409     C* Qualora nn ancora fatto imposto la prima riga con le intestazioni dei campi
037500030409     C                   IF        flgIntXLS5 = '0'
037600030409     C                   EVAL      T25XDTA = '"NUMERO ORDINE"'+','+
037700030410     C                                       '"CODICE CLIENTE TELE2"'+','+
037800030409     C                                       '"RAGIONE SOCIALE"'+','+
037900030409     C                                       '"INDIRIZZO"'+','+
038000030409     C                                       '"LOCALITA"'+','+
038100030409     C                                       '"PROVINCIA"'+','+
038200030409     C                                       '"CAP"'+','+
038300030410     C                                       '"DATA PAGAMENTO"'+','+
038400030409     C                                       '"IMPORTO CONTRASSEGNO"'+','+
038500030409     C                                       '"TIPO INCASSO"'
038600030409     C                   WRITE     TELE2X05
038700030409     C                   EVAL      flgIntXLS5 = '1'
038800030409     C                   ENDIF
038900030409     C*
039000030409     C* "Chaino" la bolla x reperire i dati mancanti
039100030409     C     KEYtas30cP    CHAIN     TITAS30C
039200030409     C                   IF        %found(TITAS30C)
039300030409     C*
039400030409     C* "Chaino" il contrassegno x reperire i dati mancanti
039500030409     C     KEYcsb03      CHAIN     TNCSB03L
039600030409     C                   IF        %found(TNCSB03L)
039700030409     C*
039800030409     C* Reperisco la descrizione del tipo incasso contrassegno
039801030429     C                   IF        VAStpi = 'M'
039802030429     C                   MOVEL(P)  'Mittente'    WdesTPI          10
039806030429     C                   ELSE
039807030429     C                   MOVEL(P)  'Bartolini'   WdesTPI
039808030429     C                   ENDIF
040700030409     C*
040800030409     C* Scrivo i record di dettaglio esiti
040900030429     C                   EVAL      T25XDTA = %trim(%editc(VASRMN:'Q'))+','+
041000030429     C                                          '"'+%trim(VASRMA)+'"'+','+
041100030429     C                                          '"'+%trim(TASRSD)+'"'+','+
041200030409     C                                          '"'+%trim(TASIND)+'"'+','+
041300030409     C                                          '"'+%trim(TASLOD)+'"'+','+
041400030409     C                                          '"'+%trim(TASPRD)+'"'+','+
041500030409     C                                          '"'+%trim(TASCAD)+'"'+','+
041600030409     C                                       %trim(%editc(CSBDDP:'Q'))+','+
041700030430     C                                       %trim(%editc(VASCAS:'P'))+','+
041800030429     C                                          '"'+%trim(WdesTPI)+'"'
041900030409     C*
042000030409     C                   WRITE     TELE2X05
042100030409     C*
042200030409     C                   ENDIF
042300030409     C                   ENDIF
042400030409     C*
042500030409     C                   ENDSR
049500030417
049600030417
049700030417
049800030417     C*----------------------------------------------------*
049900030417     C*  CONTROLLO NUMERICITA' CAMPI
050000030417     C*----------------------------------------------------*
050100030417     C     CHKNUM        BEGSR
050200030417     C*
050300030417     C                   call(e)   'ISNUMERIC'
050400030417     C                   PARM                    PiStr            30
050500030417     C                   PARM      ','           PiDecChr          1
050600030417     C                   PARM      *ZEROS        PiVal            30 9
050700030417     C                   PARM      '0'           PiInt             1
050800030417     C                   PARM      '0'           PiNum             1
050900030417     C                   IF        %error
051000030417     C                   EVAL      PiInt=*off
051100030417     C                   EVAL      PiNum=*off
051200030417     C                   ENDIF
051300030417     C*
051400030417     C                   ENDSR
051500030417     C***
051600030409
051700030410
051800030409
051900991027
052000991027      /TITLE Operazioni iniziali.
052100991027     C     *inzsr        BEGSR
052200991027     C*
052300991027     C     *ENTRY        PLIST
052400030409     C                   PARM                    prmppt
052500030409     C     wrkesito      PARM      wrkesito      prmesito
052600030409     C*
052700030409     C* IMPOSTA LA DATA CORRENTE
052800030409     C                   TIME                    N14              14 0
052900030409     C                   MOVE      N14           N8                8 0
053000030409     C                   Z-ADD     N8            G08DAT
053100030409     C                   Z-ADD     *ZEROS        G08INV
053200030409     C                   MOVEL     '0'           G08ERR
053300030409     C                   CALL      'XSRDA8'
053400030409     C                   PARM                    WLBDA8
053500030409     C                   Z-ADD     G08INV        DATCOR            8 0
053900030409     C****
054000030409     C* Definizione chiavi
054100030409     C****
055700030409     C* Chiave su TNCSB03L - completa
055800030409     C     KEYcsb03      KLIST
055900030429     C                   KFLD                    vasAAS
056000030429     C                   KFLD                    vasLNP
056100030429     C                   KFLD                    vasNRS
056200030429     C                   KFLD                    vasNSP
056300030409     C* Chiave su TITAS30C - parziale
056400030409     C     KEYtas30cP    KLIST
056500030429     C                   KFLD                    vasAAS
056600030429     C                   KFLD                    vasLNP
056700030429     C                   KFLD                    vasNRS
056800030429     C                   KFLD                    vasNSP
056900030409     C* Chiave su TIVIR01L - completa
057000030409     C     KEYvir01      KLIST
057100030409     C                   KFLD                    virKSC
057200030409     C                   KFLD                    virSEQ
057800991027     C*
057900991027     C                   ENDSR
