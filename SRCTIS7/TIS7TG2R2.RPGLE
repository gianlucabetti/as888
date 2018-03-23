000100100610     H DECEDIT('0,') DATEDIT(*DMY.)
000200090323     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300991027
000400040628     FTITAS30C  IF   E           K DISK
000500050722     FFIAR531C  IF   E           K DISK
000600050908     FTIVGDTMP  UF   E             DISK
000700100610     FTIVAWWWT  UF A E             DISK    commit
000800120221     Ftigcp52l  if   e           k disk
000900991027
001000991027     D prmppt          s             50
001100991027     D prmesito        s              1
001200991027     D wrkesito        s                   like(prmesito)
001300050908     D fnvagds       e ds                  extname(fnvag00t)
001400080918     D ChCSV           s             10A   varying inz(' ')
001500030325
001600050722     D dar5gen       e ds
001700040628
001800040628     D dstas           ds
001900040628     D  tasRSD
002000040628     D  tasIND
002100040628     D  tasCAD
002200040628     D  tasLOD
002300040628     D  tasPRD
002400040628     D  tasNZD
002500050722
002600050722     D dsar5           ds
002700050722     D  ar5UNI
002800991027
002900030908     C                   EXSR      chkRECFILMBR
003000991027     C                   EXSR      traduci
003100921023     C                   SETON                                        LR
003200030908
003300030908     C     chkRECFILMBR  BEGSR
003400030908     C*
003500100610     C                   READ      TIVAWWWT                               55
003600030908     C*
003700030908     C                   ENDSR
003800991027
003900991027     C     traduci       BEGSR
004000080918     C*
004100080918     C* Verifico se richiesto 1 particolare carattere separatore campo
004200080923     C                   IF        %subst(prmppt:4:1) = *blanks
004300100610     C                   EVAL      ChCSV = ';'
004400080918     C                   ELSE
004500080923     C                   EVAL      ChCSV = %trim(%subst(prmppt:4:1))
004600080918     C                   ENDIF
004700120221     C*
004800120221     C* Se richiesto nei parametri gestisco unicamente gicaneze aperte in attesa d disposiz.
004900120221     C                   SETOFF                                       50
005000120221     C                   IF        %subst(prmppt:6:1) = 'A'
005100120221     C                   SETON                                        50
005200120221     C                   ENDIF
005300030325     C*
005400030325     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
005500110909     C*
005600110909     C* - NOMI CAMPO
005700030908     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
005800100610     C                   EVAL      VAWDTA  = '"VAGLNP"'+ChCSV+
005900080918     C                                       '"VAGAAS"'+ChCSV+
006000080918     C                                       '"VAGNRS"'+ChCSV+
006100080918     C                                       '"VAGNSP"'+ChCSV+
006200080918     C                                       '"VAGLNA"'+ChCSV+
006300080918     C                                       '"VAGAGC"'+ChCSV+
006400080918     C                                       '"VAGMGC"'+ChCSV+
006500080918     C                                       '"VAGFGC"'+ChCSV+
006600080918     C                                       '"VAGNGC"'+ChCSV+
006700080918     C                                       '"VAGFRG"'+ChCSV+
006800080918     C                                       '"VAGGGA"'+ChCSV+
006900080918     C                                       '"VAGCMC"'+ChCSV+
007000080918     C                                       '"VAGDMC"'+ChCSV+
007100080918     C                                       '"VAGNOT"'+ChCSV+
007200080918     C                                       '"VAGRMN"'+ChCSV+
007300080918     C                                       '"VAGRMA"'+ChCSV+
007400080918     C                                       '"VAGSCM"'+ChCSV+
007500080918     C                                       '"VAGDMM"'+ChCSV+
007600080918     C                                       '"VAGVCS"'+ChCSV+
007700080918     C                                       '"VAGCAS"'+ChCSV+
007800080918     C                                       '"VAGVCA"'+ChCSV+
007900080918     C                                       '"VAGDDM"'+ChCSV+
008000080918     C                                       '"VAGDED"'+ChCSV+
008100080918     C                                       '"VAGDUR"'+ChCSV+
008200080918     C                                       '"VAGDCG"'+ChCSV+
008300080918     C                                       '"VAGCFG"'+ChCSV+
008400080918     C                                       '"VAGSGS"'+ChCSV+
008500080918     C                                       '"VAGSGR"'+ChCSV+
008600080918     C                                       '"VAGSGD"'+ChCSV+
008700080918     C                                       '"VAGDSD"'+ChCSV+
008800080918     C                                       '"VAGDSA"'+ChCSV+
008900080918     C                                       '"VAGGGS"'+ChCSV+
009000080918     C                                       '"VAGSGF"'+ChCSV+
009100080918     C                                       '"VAGSGA"'+ChCSV+
009200080918     C                                       '"VAGTFT"'+ChCSV+
009300080918     C                                       '"VAGDBG"'+ChCSV+
009400080918     C                                       '"VAGLPW"'+ChCSV+
009500080918     C                                       '"VAGASW"'+ChCSV+
009600080918     C                                       '"VAGNSW"'+ChCSV+
009700080918     C                                       '"VAGNBW"'+ChCSV+
009800120221     C                                       '"VAGKSC"'
009900040628     C*
010000040628     C* Se richiesto nei parametri aggingo nelle intestazioni colonne anche i campi del destinatario
010100040628     C                   IF        %subst(prmppt:3:1) = 'D'
010200100610     C                   EVAL      VAWDTA  = %trim(VAWDTA) +ChCSV+
010300080918     C                                           '"VAGRSD"'+ChCSV+
010400080918     C                                           '"VAGIND"'+ChCSV+
010500080918     C                                           '"VAGCAD"'+ChCSV+
010600080918     C                                           '"VAGLOD"'+ChCSV+
010700080918     C                                           '"VAGPRD"'+ChCSV+
010800080918     C                                           '"VAGNZD"'+ChCSV+
010900080918     C                                           '"VAGTEL"'
011000040628     C                   ENDIF
011100120221     C*
011200120221     C* Se richiesto nei parametri aggingo nelle intestazioni colonne anche la fase
011300120221     C                   IF        %subst(prmppt:5:1) = 'F'
011400120221     C                   EVAL      VAWDTA  = %trim(VAWDTA) +ChCSV+
011500120221     C                                           '"VAGFAS"'
011600120221     C                   ENDIF
011700030325     C*
011800100610     C                   WRITE     TIVAW000
011900030325     C                   ENDIF
012000110909     C*
012100110909     C* - DESCRIZIONI CAMPO - ITALIANO
012200110909     C                   IF        %subst(prmppt:1:1) = 'D' AND *IN55 = *ON
012300110909     C                   EVAL      VAWDTA  =
012400110909     C                             '"FILIALE PARTENZA"'                  +ChCSV+
012500110909     C                             '"ANNO SPEDIZIONE"'                   +ChCSV+
012600110909     C                             '"NUMERO SERIE"'                      +ChCSV+
012700110909     C                             '"NUMERO SPEDIZIONE"'                 +ChCSV+
012800110909     C                             '"FILIALE ARRIVO"'                    +ChCSV+
012900110909     C                             '"ANNO APERTURA GIACENZA"'            +ChCSV+
013000110909     C                             '"MESE/GIORNO APERTURA GIACENZA"'     +ChCSV+
013100110909     C                             '"FILIALE CHE HA APERTO LA GIACENZA"' +ChCSV+
013200110909     C                             '"NUMERO GIACENZA"'                   +ChCSV+
013300110909     C                             '"NR. PROGR. RIAPERT. GIACENZA"'      +ChCSV+
013400110909     C                             '"GESTIONE PARTICOLARITA GIACENZA"'   +ChCSV+
013500110909     C                             '"CAUSALE GIACENZA"'                  +ChCSV+
013600110909     C                             '"DESCRIZIONE CAUSALE GIACENZA"'      +ChCSV+
013700110909     C                             '"ULTERIORE MOTIVO DI GIACENZA"'      +ChCSV+
013800110909     C                             '"RIFERIM.MITTENTE NUMERICO"'         +ChCSV+
013900110909     C                             '"RIFERIM.MITTENTE ALFABETICO"'       +ChCSV+
014000110909     C                             '"COD.  MITTENTE GIACENZA"'           +ChCSV+
014100110909     C                             '"DATA ST.MODULO MITTENTE GIACENZA"'  +ChCSV+
014200110909     C                             '"VARIAZ.IMPORTO C/ASS -S/N"'         +ChCSV+
014300110909     C                             '"NUOVO IMPORTO C/ASSEGNO"'           +ChCSV+
014400110909     C                             '"DIVISA IMPORTO C/ASSEGNO"'          +ChCSV+
014500110909     C                             '"DATA DISPOSIZIONI MITTENTE"'        +ChCSV+
014600110909     C                             '"DATA ESEGUIBILITA DISPOS. GIAC."'   +ChCSV+
014700110909     C                             '"ULTIMA DATA RIAPERT.GIACENZA"'      +ChCSV+
014800110909     C                             '"DATA CHIUSURA GIACENZA IN ARRIVO"'  +ChCSV+
014900110909     C                             '"CODICE FINE GIACENZA"'              +ChCSV+
015000110909     C                             '"SPESE SOSTA GIACENZA"'              +ChCSV+
015100110909     C                             '"SPESE RICONS./VARIE GIACENZA"'      +ChCSV+
015200110909     C                             '"SPESE DOSSIER GIACENZA"'            +ChCSV+
015300110909     C                             '"DATA SOSTA -DAL-"'                  +ChCSV+
015400110909     C                             '"DATA SOSTA -AL-"'                   +ChCSV+
015500110909     C                             '"GIORNI SOSTA"'                      +ChCSV+
015600110909     C                             '"FRANCHIGIA GIORNI SOSTA"'           +ChCSV+
015700110909     C                             '"SPESE AGGIUNTIVE GIACENZA"'         +ChCSV+
015800110909     C                             '"TIPO FATTURAZ.: 0=SI 9=NO"'         +ChCSV+
015900110909     C                             '"DATA CREAZ. BOLLA GIACENZA"'        +ChCSV+
016000110909     C                             '"FIL.PART. BOLLA GIACENZA"'          +ChCSV+
016100110909     C                             '"ANNO BOLLA GIACENZA"'               +ChCSV+
016200110909     C                             '"NUM.SERIE SPED. BOLLA GIACENZA"'    +ChCSV+
016300110909     C                             '"NUM.SPEDIZ. BOLLA GIACENZA"'        +ChCSV+
016400110909     C                             '"COD.CLIENTE BOLLA ADDEBITO"'
016500110909     C*
016600110909     C* Se richiesto nei parametri aggingo nelle intestazioni colonne anche i campi del destinatario
016700110909     C                   IF        %subst(prmppt:3:1) = 'D'
016800110909     C                   EVAL      VAWDTA  =
016900110909     C                             %trim(VAWDTA)                         +ChCSV+
017000110909     C                             '"RAGIONE SOCIALE DESTINATARIO"'      +ChCSV+
017100110909     C                             '"INDIRIZZO DESTINATARIO"'            +ChCSV+
017200110909     C                             '"CAP DESTINATARIO"'                  +ChCSV+
017300110909     C                             '"LOCALITA DESTINATARIO"'             +ChCSV+
017400110909     C                             '"PROVINCIA DESTINATARIO"'            +ChCSV+
017500110909     C                             '"NAZIONE DESTINATARIO"'              +ChCSV+
017600110909     C                             '"TELEFONO DESTINATARIO"'
017700110909     C                   ENDIF
017800120221     C*
017900120221     C* Se richiesto nei parametri aggingo nelle intestazioni colonne anche la fase
018000120221     C                   IF        %subst(prmppt:5:1) = 'F'
018100120221     C                   EVAL      VAWDTA  = %trim(VAWDTA) +ChCSV+
018200120221     C                                           '"FASE"'
018300120221     C                   ENDIF
018400110909     C*
018500120221     C                   WRITE     TIVAW000
018600110909     C                   ENDIF
018700991027     C*
018800050908     C                   READ      TIVGDTMP
018900050908     C                   DOW       not %eof(TIVGDTMP)
019000050908     C                   eval      fnvagds = vgdDTA
019100991027     C*
019200030325     C* Verifico che nn vi siano nei campi alfa caratteri <"> che altrimenti sostituisco con <'>
019300060411     C***  '"':''''      XLATE     fnvagds       fnvagds
019400040628     C*
019500040628     C* Se richiesto nei parametri anche i campi del destinatario "chaino" la bolla relativa alla
019600040628     C* giacenza corrente.
019700040628     C                   IF        %subst(prmppt:3:1) = 'D'
019800050722     C                   CLEAR                   dstas
019900050722     C                   CLEAR                   dsar5
020000050722     C                   CLEAR                   dar5gen
020100040628     C     KEYTAS30_1P   CHAIN     TITAS30C
020200040628     C                   IF        %found(TITAS30C)
020300050722     C                   EVAL      AR5TRD = 'GEN'
020400050722     C     KEYAR531_1P   CHAIN     FIAR531C
020500050722     C                   IF        %found(FIAR531C)
020600040628     C* Verifico che nn vi siano nei campi alfa caratteri <"> che altrimenti sostituisco con <'>
020700060411     C***  '"':''''      XLATE     dstas         dstas
020800060411     C***  '"':''''      XLATE     dsar5         dsar5
020900050722     C                   EVAL      dar5gen = ar5UNI
021000040628     C                   ENDIF
021100050722     C                   ENDIF
021200040628     C                   ENDIF
021300120221     C*
021400120221     C* Se richiesto nei parametri => solo giacenze in attesa d disposizioni
021500120221     C                   setoff                                       31
021600120221     C                   if        %subst(prmppt:5:1) = 'F'
021700120221     C                   eval      gcpFRG = vagFRG
021800120221     C     KEYGCP52_C    chain     tigcp52l
021900120221     C                   if        %found(tigcp52l)
022000120221     C                   if        gcpfas >= 020 AND
022100120221     C                             gcpfas <  025
022200120221     C   50              seton                                        31
022300120221     C                   endif
022400120221     C                   endif
022500120221     C                   endif
022600030325     C*
022700040204     C* Se richiesto nei parametri l'editazione dei campi data => gestisco
022800040204     C                   IF        %subst(prmppt:2:1) = 'D'
022900100610     C                   EVAL      VAWDTA  = %trim(%editc(VAGLNP:'Q'))+ChCSV+
023000080918     C                                       %trim(%editc(VAGAAS:'Q'))+ChCSV+
023100080918     C                                       %trim(%editc(VAGNRS:'Q'))+ChCSV+
023200080918     C                                       %trim(%editc(VAGNSP:'Q'))+ChCSV+
023300080918     C                                       %trim(%editc(VAGLNA:'Q'))+ChCSV+
023400080918     C                                       %trim(%editc(VAGAGC:'Q'))+ChCSV+
023500080918     C                                       %trim(%editc(VAGMGC:'Q'))+ChCSV+
023600080918     C                                       %trim(%editc(VAGFGC:'Q'))+ChCSV+
023700080918     C                                       %trim(%editc(VAGNGC:'Q'))+ChCSV+
023800080918     C                                       %trim(%editc(VAGFRG:'Q'))+ChCSV+
023900080918     C                                           '"'+%trim(VAGGGA)+'"'+ChCSV+
024000080918     C                                           '"'+%trim(VAGCMC)+'"'+ChCSV+
024100080918     C                                           '"'+%trim(VAGDMC)+'"'+ChCSV+
024200080918     C                                           '"'+%trim(VAGNOT)+'"'+ChCSV+
024300080918     C                                       %trim(%editc(VAGRMN:'Q'))+ChCSV+
024400080918     C                                           '"'+%trim(VAGRMA)+'"'+ChCSV+
024500080918     C                                       %trim(%editc(VAGSCM:'Q'))+ChCSV+
024600080918     C                              %trim(%editw(VAGDMM:'    /  /  '))+ChCSV+
024700080918     C                                           '"'+%trim(VAGVCS)+'"'+ChCSV+
024800080918     C                                       %trim(%editc(VAGCAS:'Q'))+ChCSV+
024900080918     C                                           '"'+%trim(VAGVCA)+'"'+ChCSV+
025000080918     C                              %trim(%editw(VAGDDM:'    /  /  '))+ChCSV+
025100080918     C                              %trim(%editw(VAGDED:'    /  /  '))+ChCSV+
025200080918     C                              %trim(%editw(VAGDUR:'    /  /  '))+ChCSV+
025300080918     C                              %trim(%editw(VAGDCG:'    /  /  '))+ChCSV+
025400080918     C                                           '"'+%trim(VAGCFG)+'"'+ChCSV+
025500080918     C                                       %trim(%editc(VAGSGS:'Q'))+ChCSV+
025600080918     C                                       %trim(%editc(VAGSGR:'Q'))+ChCSV+
025700080918     C                                       %trim(%editc(VAGSGD:'Q'))+ChCSV+
025800080918     C                              %trim(%editw(VAGDSD:'    /  /  '))+ChCSV+
025900080918     C                              %trim(%editw(VAGDSA:'    /  /  '))+ChCSV+
026000080918     C                                       %trim(%editc(VAGGGS:'Q'))+ChCSV+
026100080918     C                                       %trim(%editc(VAGSGF:'Q'))+ChCSV+
026200080918     C                                       %trim(%editc(VAGSGA:'Q'))+ChCSV+
026300080918     C                                       %trim(%editc(VAGTFT:'Q'))+ChCSV+
026400080918     C                              %trim(%editw(VAGDBG:'    /  /  '))+ChCSV+
026500080918     C                                       %trim(%editc(VAGLPW:'Q'))+ChCSV+
026600080918     C                                       %trim(%editc(VAGASW:'Q'))+ChCSV+
026700080918     C                                       %trim(%editc(VAGNSW:'Q'))+ChCSV+
026800080918     C                                       %trim(%editc(VAGNBW:'Q'))+ChCSV+
026900080918     C                                       %trim(%editc(VAGKSC:'Q'))
027000080918     C*
027100040628     C* Se richiesto nei parametri aggiungo anche i campi del destinatario
027200040628     C                   IF        %subst(prmppt:3:1) = 'D'
027300100610     C                   EVAL      VAWDTA  =        %trim(VAWDTA)    +ChCSV+
027400080918     C                                          '"'+%trim(TASRSD)+'"'+ChCSV+
027500080918     C                                          '"'+%trim(TASIND)+'"'+ChCSV+
027600080918     C                                          '"'+%trim(TASCAD)+'"'+ChCSV+
027700080918     C                                          '"'+%trim(TASLOD)+'"'+ChCSV+
027800080918     C                                          '"'+%trim(TASPRD)+'"'+ChCSV+
027900080918     C                                          '"'+%trim(TASNZD)+'"'+ChCSV+
028000080918     C                                        '"'+%trim(§AR5TELD)+'"'
028100080918     C                   ENDIF
028200120221     C*
028300120221     C* Se richiesto nei parametri aggiungo anche il campo della fase
028400120221     C                   IF        %subst(prmppt:5:1) = 'F'
028500120221     C                   EVAL      VAWDTA  = %trim(VAWDTA)               +ChCSV+
028600120221     C                                  '"'+%trim(%editc(gcpFAS:'X'))+'"'+ChCSV
028700120221     C                   ENDIF
028800040628     C*
028900040204     C                   ELSE
029000040204     C*
029100100610     C                   EVAL      VAWDTA  = %trim(%editc(VAGLNP:'Q'))+ChCSV+
029200080918     C                                       %trim(%editc(VAGAAS:'Q'))+ChCSV+
029300080918     C                                       %trim(%editc(VAGNRS:'Q'))+ChCSV+
029400080918     C                                       %trim(%editc(VAGNSP:'Q'))+ChCSV+
029500080918     C                                       %trim(%editc(VAGLNA:'Q'))+ChCSV+
029600080918     C                                       %trim(%editc(VAGAGC:'Q'))+ChCSV+
029700080918     C                                       %trim(%editc(VAGMGC:'Q'))+ChCSV+
029800080918     C                                       %trim(%editc(VAGFGC:'Q'))+ChCSV+
029900080918     C                                       %trim(%editc(VAGNGC:'Q'))+ChCSV+
030000080918     C                                       %trim(%editc(VAGFRG:'Q'))+ChCSV+
030100080918     C                                           '"'+%trim(VAGGGA)+'"'+ChCSV+
030200080918     C                                           '"'+%trim(VAGCMC)+'"'+ChCSV+
030300080918     C                                           '"'+%trim(VAGDMC)+'"'+ChCSV+
030400080918     C                                           '"'+%trim(VAGNOT)+'"'+ChCSV+
030500080918     C                                       %trim(%editc(VAGRMN:'Q'))+ChCSV+
030600080918     C                                           '"'+%trim(VAGRMA)+'"'+ChCSV+
030700080918     C                                       %trim(%editc(VAGSCM:'Q'))+ChCSV+
030800080918     C                                       %trim(%editc(VAGDMM:'Q'))+ChCSV+
030900080918     C                                           '"'+%trim(VAGVCS)+'"'+ChCSV+
031000080918     C                                       %trim(%editc(VAGCAS:'Q'))+ChCSV+
031100080918     C                                           '"'+%trim(VAGVCA)+'"'+ChCSV+
031200080918     C                                       %trim(%editc(VAGDDM:'Q'))+ChCSV+
031300080918     C                                       %trim(%editc(VAGDED:'Q'))+ChCSV+
031400080918     C                                       %trim(%editc(VAGDUR:'Q'))+ChCSV+
031500080918     C                                       %trim(%editc(VAGDCG:'Q'))+ChCSV+
031600080918     C                                           '"'+%trim(VAGCFG)+'"'+ChCSV+
031700080918     C                                       %trim(%editc(VAGSGS:'Q'))+ChCSV+
031800080918     C                                       %trim(%editc(VAGSGR:'Q'))+ChCSV+
031900080918     C                                       %trim(%editc(VAGSGD:'Q'))+ChCSV+
032000080918     C                                       %trim(%editc(VAGDSD:'Q'))+ChCSV+
032100080918     C                                       %trim(%editc(VAGDSA:'Q'))+ChCSV+
032200080918     C                                       %trim(%editc(VAGGGS:'Q'))+ChCSV+
032300080918     C                                       %trim(%editc(VAGSGF:'Q'))+ChCSV+
032400080918     C                                       %trim(%editc(VAGSGA:'Q'))+ChCSV+
032500080918     C                                       %trim(%editc(VAGTFT:'Q'))+ChCSV+
032600080918     C                                       %trim(%editc(VAGDBG:'Q'))+ChCSV+
032700080918     C                                       %trim(%editc(VAGLPW:'Q'))+ChCSV+
032800080918     C                                       %trim(%editc(VAGASW:'Q'))+ChCSV+
032900080918     C                                       %trim(%editc(VAGNSW:'Q'))+ChCSV+
033000080918     C                                       %trim(%editc(VAGNBW:'Q'))+ChCSV+
033100030331     C                                       %trim(%editc(VAGKSC:'Q'))
033200040628     C*
033300040628     C* Se richiesto nei parametri aggiungo anche i campi del destinatario
033400040628     C                   IF        %subst(prmppt:3:1) = 'D'
033500100702     C                   EVAL      VAWDTA  =        %trim(VAWDTA)    +ChCSV+
033600080918     C                                          '"'+%trim(TASRSD)+'"'+ChCSV+
033700080918     C                                          '"'+%trim(TASIND)+'"'+ChCSV+
033800080918     C                                          '"'+%trim(TASCAD)+'"'+ChCSV+
033900080918     C                                          '"'+%trim(TASLOD)+'"'+ChCSV+
034000080918     C                                          '"'+%trim(TASPRD)+'"'+ChCSV+
034100080918     C                                          '"'+%trim(TASNZD)+'"'+ChCSV+
034200080918     C                                        '"'+%trim(§AR5TELD)+'"'
034300080918     C                   ENDIF
034400120221     C*
034500120221     C* Se richiesto nei parametri aggiungo anche il campo della fase
034600120221     C                   IF        %subst(prmppt:5:1) = 'F'
034700120221     C                   EVAL      VAWDTA  = %trim(VAWDTA)               +ChCSV+
034800120221     C                                  '"'+%trim(%editc(gcpFAS:'X'))+'"'+ChCSV
034900120221     C                   ENDIF
035000080918     C*
035100040204     C                   ENDIF
035200930409     C*
035300120221     C  N31              WRITE     TIVAW000
035400050908     C                   DELETE    TIVGD000
035500991027     C*
035600050908     C                   READ      TIVGDTMP
035700030325     C                   ENDDO
035800991027     C*
035900030325     C                   EVAL      wrkesito = '0'
036000991027     C*
036100910830     C                   ENDSR
036200991027
036300991027      /TITLE Operazioni iniziali.
036400991027     C     *inzsr        BEGSR
036500991027     C*
036600991027     C     *ENTRY        PLIST
036700040628     C                   PARM                    prmppt
036800040628     C     wrkesito      PARM      wrkesito      prmesito
036900991027     C*
037000040628     C* DEFINIZIONE CHIAVI
037100040628     C*
037200040628     C* Chiave su TITAS30C - Parziale
037300040628     C     KEYTAS30_1P   KLIST
037400040628     C                   KFLD                    VAGAAS
037500040628     C                   KFLD                    VAGLNP
037600040628     C                   KFLD                    VAGNRS
037700040628     C                   KFLD                    VAGNSP
037800050722     C*
037900050722     C* Chiave su FIAR531C - Parziale
038000050722     C     KEYAR531_1P   KLIST
038100050722     C                   KFLD                    VAGAAS
038200050722     C                   KFLD                    VAGLNP
038300050722     C                   KFLD                    VAGNRS
038400050722     C                   KFLD                    VAGNSP
038500050722     C                   KFLD                    AR5TRD
038600120221     C*
038700120221     C* Chiave su TIGCP52L - Completa
038800120221     C     KEYGCP52_C    KLIST
038900120221     C                   KFLD                    VAGAGC
039000120221     C                   KFLD                    VAGFGC
039100120221     C                   KFLD                    VAGNGC
039200120221     C                   KFLD                    GCPFRG
039300120221     C*
039400991027     C                   ENDSR
