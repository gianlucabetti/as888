000100100610     H DECEDIT('0,') DATEDIT(*DMY.)
000200090323     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300991027
000400040628     FTITAS30C  IF   E           K DISK
000500050722     FFIAR531C  IF   E           K DISK
000600050908     FTIVGDTMP  UF   E             DISK
000700100610     FTIVAWWWT  UF A E             DISK    commit
000800991027
000900991027     D prmppt          s             50
001000991027     D prmesito        s              1
001100991027     D wrkesito        s                   like(prmesito)
001200050908     D fnvagds       e ds                  extname(fnvag00t)
001300080918     D ChCSV           s             10A   varying inz(' ')
001400030325
001500050722     D dar5gen       e ds
001600040628
001700040628     D dstas           ds
001800040628     D  tasRSD
001900040628     D  tasIND
002000040628     D  tasCAD
002100040628     D  tasLOD
002200040628     D  tasPRD
002300040628     D  tasNZD
002400050722
002500050722     D dsar5           ds
002600050722     D  ar5UNI
002700991027
002800030908     C                   EXSR      chkRECFILMBR
002900991027     C                   EXSR      traduci
003000921023     C                   SETON                                        LR
003100030908
003200030908     C     chkRECFILMBR  BEGSR
003300030908     C*
003400100610     C                   READ      TIVAWWWT                               55
003500030908     C*
003600030908     C                   ENDSR
003700991027
003800991027     C     traduci       BEGSR
003900080918     C*
004000080918     C* Verifico se richiesto 1 particolare carattere separatore campo
004100080923     C                   IF        %subst(prmppt:4:1) = *blanks
004200100610     C                   EVAL      ChCSV = ';'
004300080918     C                   ELSE
004400080923     C                   EVAL      ChCSV = %trim(%subst(prmppt:4:1))
004500080918     C                   ENDIF
004600030325     C*
004700030325     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
004800110909     C*
004900110909     C* - NOMI CAMPO
005000030908     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
005100100610     C                   EVAL      VAWDTA  = '"VAGLNP"'+ChCSV+
005200080918     C                                       '"VAGAAS"'+ChCSV+
005300080918     C                                       '"VAGNRS"'+ChCSV+
005400080918     C                                       '"VAGNSP"'+ChCSV+
005500080918     C                                       '"VAGLNA"'+ChCSV+
005600080918     C                                       '"VAGAGC"'+ChCSV+
005700080918     C                                       '"VAGMGC"'+ChCSV+
005800080918     C                                       '"VAGFGC"'+ChCSV+
005900080918     C                                       '"VAGNGC"'+ChCSV+
006000080918     C                                       '"VAGFRG"'+ChCSV+
006100080918     C                                       '"VAGGGA"'+ChCSV+
006200080918     C                                       '"VAGCMC"'+ChCSV+
006300080918     C                                       '"VAGDMC"'+ChCSV+
006400080918     C                                       '"VAGNOT"'+ChCSV+
006500080918     C                                       '"VAGRMN"'+ChCSV+
006600080918     C                                       '"VAGRMA"'+ChCSV+
006700080918     C                                       '"VAGSCM"'+ChCSV+
006800080918     C                                       '"VAGDMM"'+ChCSV+
006900080918     C                                       '"VAGVCS"'+ChCSV+
007000080918     C                                       '"VAGCAS"'+ChCSV+
007100080918     C                                       '"VAGVCA"'+ChCSV+
007200080918     C                                       '"VAGDDM"'+ChCSV+
007300080918     C                                       '"VAGDED"'+ChCSV+
007400080918     C                                       '"VAGDUR"'+ChCSV+
007500080918     C                                       '"VAGDCG"'+ChCSV+
007600080918     C                                       '"VAGCFG"'+ChCSV+
007700080918     C                                       '"VAGSGS"'+ChCSV+
007800080918     C                                       '"VAGSGR"'+ChCSV+
007900080918     C                                       '"VAGSGD"'+ChCSV+
008000080918     C                                       '"VAGDSD"'+ChCSV+
008100080918     C                                       '"VAGDSA"'+ChCSV+
008200080918     C                                       '"VAGGGS"'+ChCSV+
008300080918     C                                       '"VAGSGF"'+ChCSV+
008400080918     C                                       '"VAGSGA"'+ChCSV+
008500080918     C                                       '"VAGTFT"'+ChCSV+
008600080918     C                                       '"VAGDBG"'+ChCSV+
008700080918     C                                       '"VAGLPW"'+ChCSV+
008800080918     C                                       '"VAGASW"'+ChCSV+
008900080918     C                                       '"VAGNSW"'+ChCSV+
009000080918     C                                       '"VAGNBW"'+ChCSV+
009100030331     C                                       '"VAGKSC"'
009200040628     C*
009300040628     C* Se richiesto nei parametri aggingo nelle intestazioni colonne anche i campi del destinatario
009400040628     C                   IF        %subst(prmppt:3:1) = 'D'
009500100610     C                   EVAL      VAWDTA  = %trim(VAWDTA) +ChCSV+
009600080918     C                                           '"VAGRSD"'+ChCSV+
009700080918     C                                           '"VAGIND"'+ChCSV+
009800080918     C                                           '"VAGCAD"'+ChCSV+
009900080918     C                                           '"VAGLOD"'+ChCSV+
010000080918     C                                           '"VAGPRD"'+ChCSV+
010100080918     C                                           '"VAGNZD"'+ChCSV+
010200080918     C                                           '"VAGTEL"'
010300040628     C                   ENDIF
010400030325     C*
010500100610     C                   WRITE     TIVAW000
010600030325     C                   ENDIF
010700110909     C*
010800110909     C* - DESCRIZIONI CAMPO - ITALIANO
010900110909     C                   IF        %subst(prmppt:1:1) = 'D' AND *IN55 = *ON
011000110909     C                   EVAL      VAWDTA  =
011100110909     C                             '"FILIALE PARTENZA"'                  +ChCSV+
011200110909     C                             '"ANNO SPEDIZIONE"'                   +ChCSV+
011300110909     C                             '"NUMERO SERIE"'                      +ChCSV+
011400110909     C                             '"NUMERO SPEDIZIONE"'                 +ChCSV+
011500110909     C                             '"FILIALE ARRIVO"'                    +ChCSV+
011600110909     C                             '"ANNO APERTURA GIACENZA"'            +ChCSV+
011700110909     C                             '"MESE/GIORNO APERTURA GIACENZA"'     +ChCSV+
011800110909     C                             '"FILIALE CHE HA APERTO LA GIACENZA"' +ChCSV+
011900110909     C                             '"NUMERO GIACENZA"'                   +ChCSV+
012000110909     C                             '"NR. PROGR. RIAPERT. GIACENZA"'      +ChCSV+
012100110909     C                             '"GESTIONE PARTICOLARITA GIACENZA"'   +ChCSV+
012200110909     C                             '"CAUSALE GIACENZA"'                  +ChCSV+
012300110909     C                             '"DESCRIZIONE CAUSALE GIACENZA"'      +ChCSV+
012400110909     C                             '"ULTERIORE MOTIVO DI GIACENZA"'      +ChCSV+
012500110909     C                             '"RIFERIM.MITTENTE NUMERICO"'         +ChCSV+
012600110909     C                             '"RIFERIM.MITTENTE ALFABETICO"'       +ChCSV+
012700110909     C                             '"COD.  MITTENTE GIACENZA"'           +ChCSV+
012800110909     C                             '"DATA ST.MODULO MITTENTE GIACENZA"'  +ChCSV+
012900110909     C                             '"VARIAZ.IMPORTO C/ASS -S/N"'         +ChCSV+
013000110909     C                             '"NUOVO IMPORTO C/ASSEGNO"'           +ChCSV+
013100110909     C                             '"DIVISA IMPORTO C/ASSEGNO"'          +ChCSV+
013200110909     C                             '"DATA DISPOSIZIONI MITTENTE"'        +ChCSV+
013300110909     C                             '"DATA ESEGUIBILITA DISPOS. GIAC."'   +ChCSV+
013400110909     C                             '"ULTIMA DATA RIAPERT.GIACENZA"'      +ChCSV+
013500110909     C                             '"DATA CHIUSURA GIACENZA IN ARRIVO"'  +ChCSV+
013600110909     C                             '"CODICE FINE GIACENZA"'              +ChCSV+
013700110909     C                             '"SPESE SOSTA GIACENZA"'              +ChCSV+
013800110909     C                             '"SPESE RICONS./VARIE GIACENZA"'      +ChCSV+
013900110909     C                             '"SPESE DOSSIER GIACENZA"'            +ChCSV+
014000110909     C                             '"DATA SOSTA -DAL-"'                  +ChCSV+
014100110909     C                             '"DATA SOSTA -AL-"'                   +ChCSV+
014200110909     C                             '"GIORNI SOSTA"'                      +ChCSV+
014300110909     C                             '"FRANCHIGIA GIORNI SOSTA"'           +ChCSV+
014400110909     C                             '"SPESE AGGIUNTIVE GIACENZA"'         +ChCSV+
014500110909     C                             '"TIPO FATTURAZ.: 0=SI 9=NO"'         +ChCSV+
014600110909     C                             '"DATA CREAZ. BOLLA GIACENZA"'        +ChCSV+
014700110909     C                             '"FIL.PART. BOLLA GIACENZA"'          +ChCSV+
014800110909     C                             '"ANNO BOLLA GIACENZA"'               +ChCSV+
014900110909     C                             '"NUM.SERIE SPED. BOLLA GIACENZA"'    +ChCSV+
015000110909     C                             '"NUM.SPEDIZ. BOLLA GIACENZA"'        +ChCSV+
015100110909     C                             '"COD.CLIENTE BOLLA ADDEBITO"'
015200110909     C*
015300110909     C* Se richiesto nei parametri aggingo nelle intestazioni colonne anche i campi del destinatario
015400110909     C                   IF        %subst(prmppt:3:1) = 'D'
015500110909     C                   EVAL      VAWDTA  =
015600110909     C                             %trim(VAWDTA)                         +ChCSV+
015700110909     C                             '"RAGIONE SOCIALE DESTINATARIO"'      +ChCSV+
015800110909     C                             '"INDIRIZZO DESTINATARIO"'            +ChCSV+
015900110909     C                             '"CAP DESTINATARIO"'                  +ChCSV+
016000110909     C                             '"LOCALITA DESTINATARIO"'             +ChCSV+
016100110909     C                             '"PROVINCIA DESTINATARIO"'            +ChCSV+
016200110909     C                             '"NAZIONE DESTINATARIO"'              +ChCSV+
016300110909     C                             '"TELEFONO DESTINATARIO"'
016400110909     C                   ENDIF
016500110909     C*
016600110909     C                   WRITE     TIVAW000
016700110909     C                   ENDIF
016800991027     C*
016900050908     C                   READ      TIVGDTMP
017000050908     C                   DOW       not %eof(TIVGDTMP)
017100050908     C                   eval      fnvagds = vgdDTA
017200991027     C*
017300030325     C* Verifico che nn vi siano nei campi alfa caratteri <"> che altrimenti sostituisco con <'>
017400060411     C***  '"':''''      XLATE     fnvagds       fnvagds
017500040628     C*
017600040628     C* Se richiesto nei parametri anche i campi del destinatario "chaino" la bolla relativa alla
017700040628     C* giacenza corrente.
017800040628     C                   IF        %subst(prmppt:3:1) = 'D'
017900050722     C                   CLEAR                   dstas
018000050722     C                   CLEAR                   dsar5
018100050722     C                   CLEAR                   dar5gen
018200040628     C     KEYTAS30_1P   CHAIN     TITAS30C
018300040628     C                   IF        %found(TITAS30C)
018400050722     C                   EVAL      AR5TRD = 'GEN'
018500050722     C     KEYAR531_1P   CHAIN     FIAR531C
018600050722     C                   IF        %found(FIAR531C)
018700040628     C* Verifico che nn vi siano nei campi alfa caratteri <"> che altrimenti sostituisco con <'>
018800060411     C***  '"':''''      XLATE     dstas         dstas
018900060411     C***  '"':''''      XLATE     dsar5         dsar5
019000050722     C                   EVAL      dar5gen = ar5UNI
019100040628     C                   ENDIF
019200050722     C                   ENDIF
019300040628     C                   ENDIF
019400030325     C*
019500040204     C* Se richiesto nei parametri l'editazione dei campi data => gestisco
019600040204     C                   IF        %subst(prmppt:2:1) = 'D'
019700100610     C                   EVAL      VAWDTA  = %trim(%editc(VAGLNP:'Q'))+ChCSV+
019800080918     C                                       %trim(%editc(VAGAAS:'Q'))+ChCSV+
019900080918     C                                       %trim(%editc(VAGNRS:'Q'))+ChCSV+
020000080918     C                                       %trim(%editc(VAGNSP:'Q'))+ChCSV+
020100080918     C                                       %trim(%editc(VAGLNA:'Q'))+ChCSV+
020200080918     C                                       %trim(%editc(VAGAGC:'Q'))+ChCSV+
020300080918     C                                       %trim(%editc(VAGMGC:'Q'))+ChCSV+
020400080918     C                                       %trim(%editc(VAGFGC:'Q'))+ChCSV+
020500080918     C                                       %trim(%editc(VAGNGC:'Q'))+ChCSV+
020600080918     C                                       %trim(%editc(VAGFRG:'Q'))+ChCSV+
020700080918     C                                           '"'+%trim(VAGGGA)+'"'+ChCSV+
020800080918     C                                           '"'+%trim(VAGCMC)+'"'+ChCSV+
020900080918     C                                           '"'+%trim(VAGDMC)+'"'+ChCSV+
021000080918     C                                           '"'+%trim(VAGNOT)+'"'+ChCSV+
021100080918     C                                       %trim(%editc(VAGRMN:'Q'))+ChCSV+
021200080918     C                                           '"'+%trim(VAGRMA)+'"'+ChCSV+
021300080918     C                                       %trim(%editc(VAGSCM:'Q'))+ChCSV+
021400080918     C                              %trim(%editw(VAGDMM:'    /  /  '))+ChCSV+
021500080918     C                                           '"'+%trim(VAGVCS)+'"'+ChCSV+
021600080918     C                                       %trim(%editc(VAGCAS:'Q'))+ChCSV+
021700080918     C                                           '"'+%trim(VAGVCA)+'"'+ChCSV+
021800080918     C                              %trim(%editw(VAGDDM:'    /  /  '))+ChCSV+
021900080918     C                              %trim(%editw(VAGDED:'    /  /  '))+ChCSV+
022000080918     C                              %trim(%editw(VAGDUR:'    /  /  '))+ChCSV+
022100080918     C                              %trim(%editw(VAGDCG:'    /  /  '))+ChCSV+
022200080918     C                                           '"'+%trim(VAGCFG)+'"'+ChCSV+
022300080918     C                                       %trim(%editc(VAGSGS:'Q'))+ChCSV+
022400080918     C                                       %trim(%editc(VAGSGR:'Q'))+ChCSV+
022500080918     C                                       %trim(%editc(VAGSGD:'Q'))+ChCSV+
022600080918     C                              %trim(%editw(VAGDSD:'    /  /  '))+ChCSV+
022700080918     C                              %trim(%editw(VAGDSA:'    /  /  '))+ChCSV+
022800080918     C                                       %trim(%editc(VAGGGS:'Q'))+ChCSV+
022900080918     C                                       %trim(%editc(VAGSGF:'Q'))+ChCSV+
023000080918     C                                       %trim(%editc(VAGSGA:'Q'))+ChCSV+
023100080918     C                                       %trim(%editc(VAGTFT:'Q'))+ChCSV+
023200080918     C                              %trim(%editw(VAGDBG:'    /  /  '))+ChCSV+
023300080918     C                                       %trim(%editc(VAGLPW:'Q'))+ChCSV+
023400080918     C                                       %trim(%editc(VAGASW:'Q'))+ChCSV+
023500080918     C                                       %trim(%editc(VAGNSW:'Q'))+ChCSV+
023600080918     C                                       %trim(%editc(VAGNBW:'Q'))+ChCSV+
023700080918     C                                       %trim(%editc(VAGKSC:'Q'))
023800080918     C*
023900040628     C* Se richiesto nei parametri aggiungo anche i campi del destinatario
024000040628     C                   IF        %subst(prmppt:3:1) = 'D'
024100100610     C                   EVAL      VAWDTA  =        %trim(VAWDTA)    +ChCSV+
024200080918     C                                          '"'+%trim(TASRSD)+'"'+ChCSV+
024300080918     C                                          '"'+%trim(TASIND)+'"'+ChCSV+
024400080918     C                                          '"'+%trim(TASCAD)+'"'+ChCSV+
024500080918     C                                          '"'+%trim(TASLOD)+'"'+ChCSV+
024600080918     C                                          '"'+%trim(TASPRD)+'"'+ChCSV+
024700080918     C                                          '"'+%trim(TASNZD)+'"'+ChCSV+
024800080918     C                                        '"'+%trim(§AR5TELD)+'"'
024900080918     C                   ENDIF
025000040628     C*
025100040204     C                   ELSE
025200040204     C*
025300100610     C                   EVAL      VAWDTA  = %trim(%editc(VAGLNP:'Q'))+ChCSV+
025400080918     C                                       %trim(%editc(VAGAAS:'Q'))+ChCSV+
025500080918     C                                       %trim(%editc(VAGNRS:'Q'))+ChCSV+
025600080918     C                                       %trim(%editc(VAGNSP:'Q'))+ChCSV+
025700080918     C                                       %trim(%editc(VAGLNA:'Q'))+ChCSV+
025800080918     C                                       %trim(%editc(VAGAGC:'Q'))+ChCSV+
025900080918     C                                       %trim(%editc(VAGMGC:'Q'))+ChCSV+
026000080918     C                                       %trim(%editc(VAGFGC:'Q'))+ChCSV+
026100080918     C                                       %trim(%editc(VAGNGC:'Q'))+ChCSV+
026200080918     C                                       %trim(%editc(VAGFRG:'Q'))+ChCSV+
026300080918     C                                           '"'+%trim(VAGGGA)+'"'+ChCSV+
026400080918     C                                           '"'+%trim(VAGCMC)+'"'+ChCSV+
026500080918     C                                           '"'+%trim(VAGDMC)+'"'+ChCSV+
026600080918     C                                           '"'+%trim(VAGNOT)+'"'+ChCSV+
026700080918     C                                       %trim(%editc(VAGRMN:'Q'))+ChCSV+
026800080918     C                                           '"'+%trim(VAGRMA)+'"'+ChCSV+
026900080918     C                                       %trim(%editc(VAGSCM:'Q'))+ChCSV+
027000080918     C                                       %trim(%editc(VAGDMM:'Q'))+ChCSV+
027100080918     C                                           '"'+%trim(VAGVCS)+'"'+ChCSV+
027200080918     C                                       %trim(%editc(VAGCAS:'Q'))+ChCSV+
027300080918     C                                           '"'+%trim(VAGVCA)+'"'+ChCSV+
027400080918     C                                       %trim(%editc(VAGDDM:'Q'))+ChCSV+
027500080918     C                                       %trim(%editc(VAGDED:'Q'))+ChCSV+
027600080918     C                                       %trim(%editc(VAGDUR:'Q'))+ChCSV+
027700080918     C                                       %trim(%editc(VAGDCG:'Q'))+ChCSV+
027800080918     C                                           '"'+%trim(VAGCFG)+'"'+ChCSV+
027900080918     C                                       %trim(%editc(VAGSGS:'Q'))+ChCSV+
028000080918     C                                       %trim(%editc(VAGSGR:'Q'))+ChCSV+
028100080918     C                                       %trim(%editc(VAGSGD:'Q'))+ChCSV+
028200080918     C                                       %trim(%editc(VAGDSD:'Q'))+ChCSV+
028300080918     C                                       %trim(%editc(VAGDSA:'Q'))+ChCSV+
028400080918     C                                       %trim(%editc(VAGGGS:'Q'))+ChCSV+
028500080918     C                                       %trim(%editc(VAGSGF:'Q'))+ChCSV+
028600080918     C                                       %trim(%editc(VAGSGA:'Q'))+ChCSV+
028700080918     C                                       %trim(%editc(VAGTFT:'Q'))+ChCSV+
028800080918     C                                       %trim(%editc(VAGDBG:'Q'))+ChCSV+
028900080918     C                                       %trim(%editc(VAGLPW:'Q'))+ChCSV+
029000080918     C                                       %trim(%editc(VAGASW:'Q'))+ChCSV+
029100080918     C                                       %trim(%editc(VAGNSW:'Q'))+ChCSV+
029200080918     C                                       %trim(%editc(VAGNBW:'Q'))+ChCSV+
029300030331     C                                       %trim(%editc(VAGKSC:'Q'))
029400040628     C*
029500040628     C* Se richiesto nei parametri aggiungo anche i campi del destinatario
029600040628     C                   IF        %subst(prmppt:3:1) = 'D'
029700100702     C                   EVAL      VAWDTA  =        %trim(VAWDTA)    +ChCSV+
029800080918     C                                          '"'+%trim(TASRSD)+'"'+ChCSV+
029900080918     C                                          '"'+%trim(TASIND)+'"'+ChCSV+
030000080918     C                                          '"'+%trim(TASCAD)+'"'+ChCSV+
030100080918     C                                          '"'+%trim(TASLOD)+'"'+ChCSV+
030200080918     C                                          '"'+%trim(TASPRD)+'"'+ChCSV+
030300080918     C                                          '"'+%trim(TASNZD)+'"'+ChCSV+
030400080918     C                                        '"'+%trim(§AR5TELD)+'"'
030500080918     C                   ENDIF
030600080918     C*
030700040204     C                   ENDIF
030800930409     C*
030900100610     C                   WRITE     TIVAW000
031000050908     C                   DELETE    TIVGD000
031100991027     C*
031200050908     C                   READ      TIVGDTMP
031300030325     C                   ENDDO
031400991027     C*
031500030325     C                   EVAL      wrkesito = '0'
031600991027     C*
031700910830     C                   ENDSR
031800991027
031900991027      /TITLE Operazioni iniziali.
032000991027     C     *inzsr        BEGSR
032100991027     C*
032200991027     C     *ENTRY        PLIST
032300040628     C                   PARM                    prmppt
032400040628     C     wrkesito      PARM      wrkesito      prmesito
032500991027     C*
032600040628     C* DEFINIZIONE CHIAVI
032700040628     C*
032800040628     C* Chiave su TITAS30C - Parziale
032900040628     C     KEYTAS30_1P   KLIST
033000040628     C                   KFLD                    VAGAAS
033100040628     C                   KFLD                    VAGLNP
033200040628     C                   KFLD                    VAGNRS
033300040628     C                   KFLD                    VAGNSP
033400050722     C*
033500050722     C* Chiave su FIAR531C - Parziale
033600050722     C     KEYAR531_1P   KLIST
033700050722     C                   KFLD                    VAGAAS
033800050722     C                   KFLD                    VAGLNP
033900050722     C                   KFLD                    VAGNRS
034000050722     C                   KFLD                    VAGNSP
034100050722     C                   KFLD                    AR5TRD
034200040628     C*
034300991027     C                   ENDSR
