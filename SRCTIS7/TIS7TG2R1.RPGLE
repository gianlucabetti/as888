000100100610     H DECEDIT('0,') DATEDIT(*DMY.)
000200090323     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300991027
000400040628     FTITAS30C  IF   E           K DISK
000500050722     FFIAR531C  IF   E           K DISK
000600151001     FTITAH30C  IF   E           K DISK
000700050908     FTIVGDTMP  UF   E             DISK
000800100610     FTIVAWWWT  UF A E             DISK    commit
000900991027
001000991027     D prmppt          s             50
001100991027     D prmesito        s              1
001200991027     D wrkesito        s                   like(prmesito)
001300050908     D fnvagds       e ds                  extname(fnvag00t)
001400080918     D ChCSV           s             10A   varying inz(' ')
001500151001     D wNNC            s             35
001600030325
001700050722     D dar5gen       e ds
001800040628
001900040628     D dstas           ds
002000040628     D  tasRSD
002100040628     D  tasIND
002200040628     D  tasCAD
002300040628     D  tasLOD
002400040628     D  tasPRD
002500040628     D  tasNZD
002600050722
002700050722     D dsar5           ds
002800050722     D  ar5UNI
002900991027
003000030908     C                   EXSR      chkRECFILMBR
003100991027     C                   EXSR      traduci
003200921023     C                   SETON                                        LR
003300030908
003400030908     C     chkRECFILMBR  BEGSR
003500030908     C*
003600100610     C                   READ      TIVAWWWT                               55
003700030908     C*
003800030908     C                   ENDSR
003900991027
004000991027     C     traduci       BEGSR
004100080918     C*
004200080918     C* Verifico se richiesto 1 particolare carattere separatore campo
004300080923     C                   IF        %subst(prmppt:4:1) = *blanks
004400100610     C                   EVAL      ChCSV = ';'
004500080918     C                   ELSE
004600080923     C                   EVAL      ChCSV = %trim(%subst(prmppt:4:1))
004700080918     C                   ENDIF
004800030325     C*
004900030325     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
005000110909     C*
005100110909     C* - NOMI CAMPO
005200030908     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
005300100610     C                   EVAL      VAWDTA  = '"VAGLNP"'+ChCSV+
005400080918     C                                       '"VAGAAS"'+ChCSV+
005500080918     C                                       '"VAGNRS"'+ChCSV+
005600080918     C                                       '"VAGNSP"'+ChCSV+
005700080918     C                                       '"VAGLNA"'+ChCSV+
005800080918     C                                       '"VAGAGC"'+ChCSV+
005900080918     C                                       '"VAGMGC"'+ChCSV+
006000080918     C                                       '"VAGFGC"'+ChCSV+
006100080918     C                                       '"VAGNGC"'+ChCSV+
006200080918     C                                       '"VAGFRG"'+ChCSV+
006300080918     C                                       '"VAGGGA"'+ChCSV+
006400080918     C                                       '"VAGCMC"'+ChCSV+
006500080918     C                                       '"VAGDMC"'+ChCSV+
006600080918     C                                       '"VAGNOT"'+ChCSV+
006700080918     C                                       '"VAGRMN"'+ChCSV+
006800080918     C                                       '"VAGRMA"'+ChCSV+
006900080918     C                                       '"VAGSCM"'+ChCSV+
007000080918     C                                       '"VAGDMM"'+ChCSV+
007100080918     C                                       '"VAGVCS"'+ChCSV+
007200080918     C                                       '"VAGCAS"'+ChCSV+
007300080918     C                                       '"VAGVCA"'+ChCSV+
007400080918     C                                       '"VAGDDM"'+ChCSV+
007500080918     C                                       '"VAGDED"'+ChCSV+
007600080918     C                                       '"VAGDUR"'+ChCSV+
007700080918     C                                       '"VAGDCG"'+ChCSV+
007800080918     C                                       '"VAGCFG"'+ChCSV+
007900080918     C                                       '"VAGSGS"'+ChCSV+
008000080918     C                                       '"VAGSGR"'+ChCSV+
008100080918     C                                       '"VAGSGD"'+ChCSV+
008200080918     C                                       '"VAGDSD"'+ChCSV+
008300080918     C                                       '"VAGDSA"'+ChCSV+
008400080918     C                                       '"VAGGGS"'+ChCSV+
008500080918     C                                       '"VAGSGF"'+ChCSV+
008600080918     C                                       '"VAGSGA"'+ChCSV+
008700080918     C                                       '"VAGTFT"'+ChCSV+
008800080918     C                                       '"VAGDBG"'+ChCSV+
008900080918     C                                       '"VAGLPW"'+ChCSV+
009000080918     C                                       '"VAGASW"'+ChCSV+
009100080918     C                                       '"VAGNSW"'+ChCSV+
009200080918     C                                       '"VAGNBW"'+ChCSV+
009300030331     C                                       '"VAGKSC"'
009400040628     C*
009500040628     C* Se richiesto nei parametri aggingo nelle intestazioni colonne anche i campi del destinatario
009600040628     C                   IF        %subst(prmppt:3:1) = 'D'
009700100610     C                   EVAL      VAWDTA  = %trim(VAWDTA) +ChCSV+
009800080918     C                                           '"VAGRSD"'+ChCSV+
009900080918     C                                           '"VAGIND"'+ChCSV+
010000080918     C                                           '"VAGCAD"'+ChCSV+
010100080918     C                                           '"VAGLOD"'+ChCSV+
010200080918     C                                           '"VAGPRD"'+ChCSV+
010300080918     C                                           '"VAGNZD"'+ChCSV+
010400080918     C                                           '"VAGTEL"'
010500040628     C                   ENDIF
010600151001     C*
010700151001     C* Se richiesto nei parametri l'indicazone anche del primo segnacollo cliente ("chi sono")
010800151001     C                   IF        %subst(prmppt:5:1) = 'S'
010900151001     C                   EVAL      VAWDTA  = %trim(VAWDTA) +ChCSV+
011000151001     C                                           '"SEGNAC."'
011100151001     C                   ENDIF
011200030325     C*
011300100610     C                   WRITE     TIVAW000
011400030325     C                   ENDIF
011500110909     C*
011600110909     C* - DESCRIZIONI CAMPO - ITALIANO
011700110909     C                   IF        %subst(prmppt:1:1) = 'D' AND *IN55 = *ON
011800110909     C                   EVAL      VAWDTA  =
011900110909     C                             '"FILIALE PARTENZA"'                  +ChCSV+
012000110909     C                             '"ANNO SPEDIZIONE"'                   +ChCSV+
012100110909     C                             '"NUMERO SERIE"'                      +ChCSV+
012200110909     C                             '"NUMERO SPEDIZIONE"'                 +ChCSV+
012300110909     C                             '"FILIALE ARRIVO"'                    +ChCSV+
012400110909     C                             '"ANNO APERTURA GIACENZA"'            +ChCSV+
012500110909     C                             '"MESE/GIORNO APERTURA GIACENZA"'     +ChCSV+
012600110909     C                             '"FILIALE CHE HA APERTO LA GIACENZA"' +ChCSV+
012700110909     C                             '"NUMERO GIACENZA"'                   +ChCSV+
012800110909     C                             '"NR. PROGR. RIAPERT. GIACENZA"'      +ChCSV+
012900110909     C                             '"GESTIONE PARTICOLARITA GIACENZA"'   +ChCSV+
013000110909     C                             '"CAUSALE GIACENZA"'                  +ChCSV+
013100110909     C                             '"DESCRIZIONE CAUSALE GIACENZA"'      +ChCSV+
013200110909     C                             '"ULTERIORE MOTIVO DI GIACENZA"'      +ChCSV+
013300110909     C                             '"RIFERIM.MITTENTE NUMERICO"'         +ChCSV+
013400110909     C                             '"RIFERIM.MITTENTE ALFABETICO"'       +ChCSV+
013500110909     C                             '"COD.  MITTENTE GIACENZA"'           +ChCSV+
013600110909     C                             '"DATA ST.MODULO MITTENTE GIACENZA"'  +ChCSV+
013700110909     C                             '"VARIAZ.IMPORTO C/ASS -S/N"'         +ChCSV+
013800110909     C                             '"NUOVO IMPORTO C/ASSEGNO"'           +ChCSV+
013900110909     C                             '"DIVISA IMPORTO C/ASSEGNO"'          +ChCSV+
014000110909     C                             '"DATA DISPOSIZIONI MITTENTE"'        +ChCSV+
014100110909     C                             '"DATA ESEGUIBILITA DISPOS. GIAC."'   +ChCSV+
014200110909     C                             '"ULTIMA DATA RIAPERT.GIACENZA"'      +ChCSV+
014300110909     C                             '"DATA CHIUSURA GIACENZA IN ARRIVO"'  +ChCSV+
014400110909     C                             '"CODICE FINE GIACENZA"'              +ChCSV+
014500110909     C                             '"SPESE SOSTA GIACENZA"'              +ChCSV+
014600110909     C                             '"SPESE RICONS./VARIE GIACENZA"'      +ChCSV+
014700110909     C                             '"SPESE DOSSIER GIACENZA"'            +ChCSV+
014800110909     C                             '"DATA SOSTA -DAL-"'                  +ChCSV+
014900110909     C                             '"DATA SOSTA -AL-"'                   +ChCSV+
015000110909     C                             '"GIORNI SOSTA"'                      +ChCSV+
015100110909     C                             '"FRANCHIGIA GIORNI SOSTA"'           +ChCSV+
015200110909     C                             '"SPESE AGGIUNTIVE GIACENZA"'         +ChCSV+
015300110909     C                             '"TIPO FATTURAZ.: 0=SI 9=NO"'         +ChCSV+
015400110909     C                             '"DATA CREAZ. BOLLA GIACENZA"'        +ChCSV+
015500110909     C                             '"FIL.PART. BOLLA GIACENZA"'          +ChCSV+
015600110909     C                             '"ANNO BOLLA GIACENZA"'               +ChCSV+
015700110909     C                             '"NUM.SERIE SPED. BOLLA GIACENZA"'    +ChCSV+
015800110909     C                             '"NUM.SPEDIZ. BOLLA GIACENZA"'        +ChCSV+
015900110909     C                             '"COD.CLIENTE BOLLA ADDEBITO"'
016000110909     C*
016100110909     C* Se richiesto nei parametri aggingo nelle intestazioni colonne anche i campi del destinatario
016200110909     C                   IF        %subst(prmppt:3:1) = 'D'
016300110909     C                   EVAL      VAWDTA  =
016400110909     C                             %trim(VAWDTA)                         +ChCSV+
016500110909     C                             '"RAGIONE SOCIALE DESTINATARIO"'      +ChCSV+
016600110909     C                             '"INDIRIZZO DESTINATARIO"'            +ChCSV+
016700110909     C                             '"CAP DESTINATARIO"'                  +ChCSV+
016800110909     C                             '"LOCALITA DESTINATARIO"'             +ChCSV+
016900110909     C                             '"PROVINCIA DESTINATARIO"'            +ChCSV+
017000110909     C                             '"NAZIONE DESTINATARIO"'              +ChCSV+
017100110909     C                             '"TELEFONO DESTINATARIO"'
017200110909     C                   ENDIF
017300151001     C*
017400151001     C* Se richiesto nei parametri l'indicazone anche del primo segnacollo cliente ("chi sono")
017500151001     C                   IF        %subst(prmppt:5:1) = 'S'
017600151001     C                   EVAL      VAWDTA  =
017700151001     C                             %trim(VAWDTA)                         +ChCSV+
017800151001     C                             '"SEGNACOLLO CLIENTE"'
017900151001     C                   ENDIF
018000110909     C*
018100110909     C                   WRITE     TIVAW000
018200110909     C                   ENDIF
018300991027     C*
018400050908     C                   READ      TIVGDTMP
018500050908     C                   DOW       not %eof(TIVGDTMP)
018600050908     C                   eval      fnvagds = vgdDTA
018700991027     C*
018800030325     C* Verifico che nn vi siano nei campi alfa caratteri <"> che altrimenti sostituisco con <'>
018900060411     C***  '"':''''      XLATE     fnvagds       fnvagds
019000040628     C*
019100040628     C* Se richiesto nei parametri anche i campi del destinatario "chaino" la bolla relativa alla
019200040628     C* giacenza corrente.
019300040628     C                   IF        %subst(prmppt:3:1) = 'D'
019400050722     C                   CLEAR                   dstas
019500050722     C                   CLEAR                   dsar5
019600050722     C                   CLEAR                   dar5gen
019700040628     C     KEYTAS30_1P   CHAIN     TITAS30C
019800040628     C                   IF        %found(TITAS30C)
019900050722     C                   EVAL      AR5TRD = 'GEN'
020000050722     C     KEYAR531_1P   CHAIN     FIAR531C
020100050722     C                   IF        %found(FIAR531C)
020200040628     C* Verifico che nn vi siano nei campi alfa caratteri <"> che altrimenti sostituisco con <'>
020300060411     C***  '"':''''      XLATE     dstas         dstas
020400060411     C***  '"':''''      XLATE     dsar5         dsar5
020500050722     C                   EVAL      dar5gen = ar5UNI
020600040628     C                   ENDIF
020700050722     C                   ENDIF
020800040628     C                   ENDIF
020900030325     C*
021000040204     C* Se richiesto nei parametri l'editazione dei campi data => gestisco
021100040204     C                   IF        %subst(prmppt:2:1) = 'D'
021200100610     C                   EVAL      VAWDTA  = %trim(%editc(VAGLNP:'Q'))+ChCSV+
021300080918     C                                       %trim(%editc(VAGAAS:'Q'))+ChCSV+
021400080918     C                                       %trim(%editc(VAGNRS:'Q'))+ChCSV+
021500080918     C                                       %trim(%editc(VAGNSP:'Q'))+ChCSV+
021600080918     C                                       %trim(%editc(VAGLNA:'Q'))+ChCSV+
021700080918     C                                       %trim(%editc(VAGAGC:'Q'))+ChCSV+
021800080918     C                                       %trim(%editc(VAGMGC:'Q'))+ChCSV+
021900080918     C                                       %trim(%editc(VAGFGC:'Q'))+ChCSV+
022000080918     C                                       %trim(%editc(VAGNGC:'Q'))+ChCSV+
022100080918     C                                       %trim(%editc(VAGFRG:'Q'))+ChCSV+
022200080918     C                                           '"'+%trim(VAGGGA)+'"'+ChCSV+
022300080918     C                                           '"'+%trim(VAGCMC)+'"'+ChCSV+
022400080918     C                                           '"'+%trim(VAGDMC)+'"'+ChCSV+
022500080918     C                                           '"'+%trim(VAGNOT)+'"'+ChCSV+
022600080918     C                                       %trim(%editc(VAGRMN:'Q'))+ChCSV+
022700080918     C                                           '"'+%trim(VAGRMA)+'"'+ChCSV+
022800080918     C                                       %trim(%editc(VAGSCM:'Q'))+ChCSV+
022900080918     C                              %trim(%editw(VAGDMM:'    /  /  '))+ChCSV+
023000080918     C                                           '"'+%trim(VAGVCS)+'"'+ChCSV+
023100080918     C                                       %trim(%editc(VAGCAS:'Q'))+ChCSV+
023200080918     C                                           '"'+%trim(VAGVCA)+'"'+ChCSV+
023300080918     C                              %trim(%editw(VAGDDM:'    /  /  '))+ChCSV+
023400080918     C                              %trim(%editw(VAGDED:'    /  /  '))+ChCSV+
023500080918     C                              %trim(%editw(VAGDUR:'    /  /  '))+ChCSV+
023600080918     C                              %trim(%editw(VAGDCG:'    /  /  '))+ChCSV+
023700080918     C                                           '"'+%trim(VAGCFG)+'"'+ChCSV+
023800080918     C                                       %trim(%editc(VAGSGS:'Q'))+ChCSV+
023900080918     C                                       %trim(%editc(VAGSGR:'Q'))+ChCSV+
024000080918     C                                       %trim(%editc(VAGSGD:'Q'))+ChCSV+
024100080918     C                              %trim(%editw(VAGDSD:'    /  /  '))+ChCSV+
024200080918     C                              %trim(%editw(VAGDSA:'    /  /  '))+ChCSV+
024300080918     C                                       %trim(%editc(VAGGGS:'Q'))+ChCSV+
024400080918     C                                       %trim(%editc(VAGSGF:'Q'))+ChCSV+
024500080918     C                                       %trim(%editc(VAGSGA:'Q'))+ChCSV+
024600080918     C                                       %trim(%editc(VAGTFT:'Q'))+ChCSV+
024700080918     C                              %trim(%editw(VAGDBG:'    /  /  '))+ChCSV+
024800080918     C                                       %trim(%editc(VAGLPW:'Q'))+ChCSV+
024900080918     C                                       %trim(%editc(VAGASW:'Q'))+ChCSV+
025000080918     C                                       %trim(%editc(VAGNSW:'Q'))+ChCSV+
025100080918     C                                       %trim(%editc(VAGNBW:'Q'))+ChCSV+
025200080918     C                                       %trim(%editc(VAGKSC:'Q'))
025300080918     C*
025400040628     C* Se richiesto nei parametri aggiungo anche i campi del destinatario
025500040628     C                   IF        %subst(prmppt:3:1) = 'D'
025600100610     C                   EVAL      VAWDTA  =        %trim(VAWDTA)    +ChCSV+
025700080918     C                                          '"'+%trim(TASRSD)+'"'+ChCSV+
025800080918     C                                          '"'+%trim(TASIND)+'"'+ChCSV+
025900080918     C                                          '"'+%trim(TASCAD)+'"'+ChCSV+
026000080918     C                                          '"'+%trim(TASLOD)+'"'+ChCSV+
026100080918     C                                          '"'+%trim(TASPRD)+'"'+ChCSV+
026200080918     C                                          '"'+%trim(TASNZD)+'"'+ChCSV+
026300080918     C                                        '"'+%trim(§AR5TELD)+'"'
026400080918     C                   ENDIF
026500151001     C*
026600151001     C* Se richiesto nei parametri l'indicazone anche del primo segnacollo cliente ("chi sono")
026700151001     C                   IF        %subst(prmppt:5:1) = 'S'
026800151001     C                   EXSR      REPWHOIS
026900151001     C                   ENDIF
027000040628     C*
027100040204     C                   ELSE
027200040204     C*
027300100610     C                   EVAL      VAWDTA  = %trim(%editc(VAGLNP:'Q'))+ChCSV+
027400080918     C                                       %trim(%editc(VAGAAS:'Q'))+ChCSV+
027500080918     C                                       %trim(%editc(VAGNRS:'Q'))+ChCSV+
027600080918     C                                       %trim(%editc(VAGNSP:'Q'))+ChCSV+
027700080918     C                                       %trim(%editc(VAGLNA:'Q'))+ChCSV+
027800080918     C                                       %trim(%editc(VAGAGC:'Q'))+ChCSV+
027900080918     C                                       %trim(%editc(VAGMGC:'Q'))+ChCSV+
028000080918     C                                       %trim(%editc(VAGFGC:'Q'))+ChCSV+
028100080918     C                                       %trim(%editc(VAGNGC:'Q'))+ChCSV+
028200080918     C                                       %trim(%editc(VAGFRG:'Q'))+ChCSV+
028300080918     C                                           '"'+%trim(VAGGGA)+'"'+ChCSV+
028400080918     C                                           '"'+%trim(VAGCMC)+'"'+ChCSV+
028500080918     C                                           '"'+%trim(VAGDMC)+'"'+ChCSV+
028600080918     C                                           '"'+%trim(VAGNOT)+'"'+ChCSV+
028700080918     C                                       %trim(%editc(VAGRMN:'Q'))+ChCSV+
028800080918     C                                           '"'+%trim(VAGRMA)+'"'+ChCSV+
028900080918     C                                       %trim(%editc(VAGSCM:'Q'))+ChCSV+
029000080918     C                                       %trim(%editc(VAGDMM:'Q'))+ChCSV+
029100080918     C                                           '"'+%trim(VAGVCS)+'"'+ChCSV+
029200080918     C                                       %trim(%editc(VAGCAS:'Q'))+ChCSV+
029300080918     C                                           '"'+%trim(VAGVCA)+'"'+ChCSV+
029400080918     C                                       %trim(%editc(VAGDDM:'Q'))+ChCSV+
029500080918     C                                       %trim(%editc(VAGDED:'Q'))+ChCSV+
029600080918     C                                       %trim(%editc(VAGDUR:'Q'))+ChCSV+
029700080918     C                                       %trim(%editc(VAGDCG:'Q'))+ChCSV+
029800080918     C                                           '"'+%trim(VAGCFG)+'"'+ChCSV+
029900080918     C                                       %trim(%editc(VAGSGS:'Q'))+ChCSV+
030000080918     C                                       %trim(%editc(VAGSGR:'Q'))+ChCSV+
030100080918     C                                       %trim(%editc(VAGSGD:'Q'))+ChCSV+
030200080918     C                                       %trim(%editc(VAGDSD:'Q'))+ChCSV+
030300080918     C                                       %trim(%editc(VAGDSA:'Q'))+ChCSV+
030400080918     C                                       %trim(%editc(VAGGGS:'Q'))+ChCSV+
030500080918     C                                       %trim(%editc(VAGSGF:'Q'))+ChCSV+
030600080918     C                                       %trim(%editc(VAGSGA:'Q'))+ChCSV+
030700080918     C                                       %trim(%editc(VAGTFT:'Q'))+ChCSV+
030800080918     C                                       %trim(%editc(VAGDBG:'Q'))+ChCSV+
030900080918     C                                       %trim(%editc(VAGLPW:'Q'))+ChCSV+
031000080918     C                                       %trim(%editc(VAGASW:'Q'))+ChCSV+
031100080918     C                                       %trim(%editc(VAGNSW:'Q'))+ChCSV+
031200080918     C                                       %trim(%editc(VAGNBW:'Q'))+ChCSV+
031300030331     C                                       %trim(%editc(VAGKSC:'Q'))
031400040628     C*
031500040628     C* Se richiesto nei parametri aggiungo anche i campi del destinatario
031600040628     C                   IF        %subst(prmppt:3:1) = 'D'
031700100702     C                   EVAL      VAWDTA  =        %trim(VAWDTA)    +ChCSV+
031800080918     C                                          '"'+%trim(TASRSD)+'"'+ChCSV+
031900080918     C                                          '"'+%trim(TASIND)+'"'+ChCSV+
032000080918     C                                          '"'+%trim(TASCAD)+'"'+ChCSV+
032100080918     C                                          '"'+%trim(TASLOD)+'"'+ChCSV+
032200080918     C                                          '"'+%trim(TASPRD)+'"'+ChCSV+
032300080918     C                                          '"'+%trim(TASNZD)+'"'+ChCSV+
032400080918     C                                        '"'+%trim(§AR5TELD)+'"'
032500080918     C                   ENDIF
032600151001     C*
032700151001     C* Se richiesto nei parametri l'indicazone anche del primo segnacollo cliente ("chi sono")
032800151001     C                   IF        %subst(prmppt:5:1) = 'S'
032900151001     C                   EXSR      REPWHOIS
033000151001     C                   ENDIF
033100080918     C*
033200040204     C                   ENDIF
033300930409     C*
033400100610     C                   WRITE     TIVAW000
033500050908     C                   DELETE    TIVGD000
033600991027     C*
033700050908     C                   READ      TIVGDTMP
033800030325     C                   ENDDO
033900991027     C*
034000030325     C                   EVAL      wrkesito = '0'
034100991027     C*
034200910830     C                   ENDSR
034300151001
034400151001
034500151001
034600151001      /TITLE Reperimento SEGNACOLLO CLIENTE ("chi sono")
034700151001     C     REPWHOIS      BEGSR
034800151001     C*
034900151001     C                   SETOFF                                       60
035000151001     C                   CLEAR                   wNNC
035100151001     C*
035200151001     C* Aggancio il dettaglio colli
035300151001     C                   EVAL      tahTRC = 'C'
035400151001     C     KEYtah30_C    SETLL     titah30c
035500151001     C                   IF        %equal(titah30c)
035600151001     C     KEYtah30_C    READE     titah30c
035700151001     C                   DOW       not %eof(titah30c) AND not *IN60
035800151001     C                   EVAL      wNNC = tahNOT
035900151001     C                   SETON                                        60
036000151001     C     KEYtah30_C    READE     titah30c
036100151001     C                   ENDDO
036200151001     C                   ENDIF
036300151001     C*
036400151001     C                   EVAL      VAWDTA  =        %trim(VAWDTA)    +ChCSV+
036500151001     C                                          '"'+%trim(wNNC)+'"'
036600151001     C*
036700151001     C                   ENDSR
036800151001
036900151001
037000991027
037100991027      /TITLE Operazioni iniziali.
037200991027     C     *inzsr        BEGSR
037300991027     C*
037400991027     C     *ENTRY        PLIST
037500040628     C                   PARM                    prmppt
037600040628     C     wrkesito      PARM      wrkesito      prmesito
037700991027     C*
037800040628     C* DEFINIZIONE CHIAVI
037900040628     C*
038000040628     C* Chiave su TITAS30C - Parziale
038100040628     C     KEYTAS30_1P   KLIST
038200040628     C                   KFLD                    VAGAAS
038300040628     C                   KFLD                    VAGLNP
038400040628     C                   KFLD                    VAGNRS
038500151001     C                   KFLD                    VAGNSP
038600151001     C*
038700151001     C* Chiave su TITAH30C - Completa
038800151001     C     KEYtah30_C    KLIST
038900151001     C                   KFLD                    VAGAAS
039000151001     C                   KFLD                    VAGLNP
039100151001     C                   KFLD                    VAGNRS
039200151001     C                   KFLD                    VAGNSP
039300151001     C                   KFLD                    tahTRC
039400050722     C*
039500050722     C* Chiave su FIAR531C - Parziale
039600050722     C     KEYAR531_1P   KLIST
039700050722     C                   KFLD                    VAGAAS
039800050722     C                   KFLD                    VAGLNP
039900050722     C                   KFLD                    VAGNRS
040000050722     C                   KFLD                    VAGNSP
040100050722     C                   KFLD                    AR5TRD
040200040628     C*
040300991027     C                   ENDSR
