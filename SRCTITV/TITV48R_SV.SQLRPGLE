000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200991027     H dftactgrp(*yes)
000300991027
000400030521     FFNVAC00T  uF   E             DISK    RENAME(FNVAC000:FNVACTTT)
000500030409     FTELE2XLS1 O    E             DISK
000600030409     FTELE2XLS2 O    E             DISK
000700030409     FTELE2XLS3 O    E             DISK
000800030409     FTELE2XLS4 O    E             DISK
000900030331     FFNVAC00S  O    E             DISK    rename(fnvac000:fnvacsss)
001000030409     FTABEL00F  IF   E           K DISK
001100030409     FTITAS30C  IF   E           K DISK
001200030603     FFNLBL01L  IF   E           K DISK
001300030409     FFNEVB05L  IF   E           K DISK
001400030409     FTIVIR01L  UF   E           K DISK
001500030411     FTIVLT01L  UF A E           K DISK
001600030526     Ftis7prgf  uf   e             disk    RENAME(tis7prgf:tis7prg0)
001700030526     F                                     PREFIX(f_)
001800030526     F                                     USROPN
001900991027
002000991027     D prmppt          s             50
002100991027     D prmesito        s              1
002200030521     D prmksc          s              7
002300991027     D wrkesito        s                   like(prmesito)
002400030409     D flgIntXLS1      s              1    inz('0')
002500030409     D flgIntXLS2      s              1    inz('0')
002600030409     D flgIntXLS3      s              1    inz('0')
002700030409     D flgIntXLS4      s              1    inz('0')
002800030409     D depCEV          s                   like(evbCEV)
002900030430     D depCEV_2        s                   like(evbCEV)
003000030411     D wPriorita       s              1  0 inz(*zeros)
003100030411     D wDepPriorita    s              1  0 inz(*zeros)
003200030417     D wParmGgSet      s              1  0 inz(*zeros)
003300030521     D wVACRRN         s             15  0 inz(*zeros)
003400030521     D depNSP          s                   like(vacNSP)
003500030526     D wrkoggiiso      s               d
003600030526     D wrkmese         s              2  0
003700030526     D dwlprg          s             10    INZ(*all'0')
003800030526     D wrkprg          s              8  0 INZ(*zeros)
003900030609     D wData1_iso      s               d
004000030609     D wData2_iso      s               d
004100030609     D wData2_alfa     s              8
004200030609     D wData2_num      s              8  0 INZ(*zeros)
004300030609     D wNumGg          s              3  0 INZ(*zeros)
004400030526     D                 ds                  INZ
004500030526     D wrkday                  1      2s 0
004600030526     D wrkdaychr                      2    overlay(wrkday)
004700030325
004800030409     D dsvac         e ds                  extname(fnvac00t)
004900030409     D ds2a          e ds
005000030409     D X16GGSETDS    e ds
005100030409     D sk2Ainderr      s              3    dim(5) ctdata perrcd(1)
005200030409     D sk2Arif         s              3    dim(20) ctdata perrcd(1)
005300030410     D sk2Agiac        s              3    dim(100)
005400030526     D mc              s              1    dim(12) ctdata perrcd(12)
005500030409
005600030409     D*---------------------
005700030409     D* DS "XSRDA8" - CONTROLLA DATA (8)
005800030409     D*---------------------
005900030409     D WLBDA8          DS
006000030409     D  G08DAT                 1      8  0
006100030409     D  G08INV                 9     16  0
006200030409     D  G08ERR                17     17
006300030409     D  G08TGI                18     22  0
006400030409
006500030409
006600030417     C*
006700030417     C* Verifico se ricevuti dei parametri (nella fatispecie il giorno della settimana in cui
006800030417     C* effettuare la traduzione
006900030417     C                   EVAL      wParmGgSet = 2                               * DFT = Lunedì
007000030417     C                   IF        %subst(prmppt:9:1) <> *blanks
007100030417     C                   EVAL      PiStr=%trim(%subst(prmppt:9:1))
007200030417     C                   EXSR      CHKNUM
007300030417     C                   IF        PiInt=*on
007400030417     C                   Z-ADD     PiVal         wParmGgSet
007500030417     C                   ENDIF
007600030417     C                   ENDIF
007700030409     C*
007800030409     C* Innanzitutto verifico se la traduzione è da eseguire in quanto il cliente vuole i dati
007900030609     C* solamente 1 volta a settimana (nella fattispecie abbiamo stabilito il lunedì)...
008000030609     C* ...oppure se in ogni caso dall'ultima traduzione sono passati + d 7 giorni.
008100030609     C                   EVAL      virKSC = '0' + prmksc
008200030609     C                   EVAL      virSEQ = 001
008300030609     C     KEYvir01      CHAIN     TIVIR01L
008400030609     C                   IF        %found(TIVIR01L)
008500030609     C* - verifico se sono passati + d 7 giorni dall'ultima traduzione
008600030609     C     *ISO          MOVE      DATCOR        wData1_iso
008700030609     C                   EVAL      wData2_alfa = %subst(virPPT:1:8)
008800030609     C                   MOVE      wData2_alfa   wData2_num
008900030609     C     *ISO          MOVE      wData2_num    wData2_iso
009000030609     C                   Z-ADD     *zeros        wNumGg
009100030609     C     wData1_iso    SUBDUR    wData2_iso    wNumGg:*D
009200030609     C* - verifico se trattasi del giorno stabilito
009300030409     C                   CLEAR                   X16GGSETDS
009400030409     C     *ISO          MOVE      DATCOR        X16DATA
009500030409     C                   CALL      'X16GGSET'
009600030409     C                   PARM                    X16GGSETDS
009700030609     C* Se soddisfatte almeno 1 delle 2 condizioni => eseguo traduzione
009800030609     C                   IF        X16GIORNO = wParmGgSet OR
009900030609     C                             %abs(wNumGg) > 7
010000030417     C                   IF        %subst(virPPT:1:8) <>
010100030417     C                             %editw(DATCOR:'        ')
010200030409     C                   EXSR      traduci
010300030417     C                   EVAL      %subst(virPPT:1:8) =
010400030417     C                             %editw(DATCOR:'        ')
010500030409     C                   UPDATE    TIVIR000
010600030410     C                   EXSR      crealogs
010700030409     C                   ENDIF
010800030409     C                   ENDIF
010900030409     C                   ENDIF
011000030409     C*
011100030409     C                   EVAL      wrkesito = '0'
011200030409     C*
011300921023     C                   SETON                                        LR
011400030409
011500030409
011600030409
011700991027
011800991027     C     traduci       BEGSR
011900991027     C*
012000030521     C/EXEC SQL
012100030521     C+ declare C1 cursor for
012200030521     C+ select fnvac00t.*, rrn(fnvac00t)
012300030521     C+ from fnvac00t order by vacnsp
012400030521     C+ for read only
012500030521     C/END-EXEC
012600030521     C
012700030521     C/EXEC SQL
012800030521     C+ open C1
012900030521     C/END-EXEC
013000030521     C
013100030521     C/EXEC SQL
013200030521     C+ Fetch C1 into :dsvac, :wVACRRN
013300030521     C/END-EXEC
013400030521     C*
013500030521     C                   DOW       sqlcod = *zeros
013600030521 IF 0C                   IF        vacNSP <> depNSP
013700030521     C                   EVAL      depNSP = vacNSP
013800030603     C*
013900030603     C* Testo che la bolla nn sia 1 bolla "figlia"
014000030603     C     KEYlbl01C     CHAIN     FNLBL01L
014100030603     C                   IF        %found(FNLBL01L)
014200030603     C                   ELSE
014300991027     C*
014400030325     C* Verifico che nn vi siano nei campi alfa caratteri <"> che altrimenti sostituisco con <'>
014500030331     C     '"':''''      XLATE     dsvac         dsvac
014600030715     C*
014700030715     C* Compongo la data spedizoine come unico campo alfa
014800030715     C                   MOVEL(P)  VACAAS        VACDSP            8
014900030715     C                   MOVE      VACMGS        VACDSP
015000030409     C*
015100030410     C* Innanzitutto elaboro solo le bolle "chiuse" con reso e aventi una apertura giacenza
015200030410 IF 1C                   IF        vacDCM > *zeros AND
015300030429     C                             vacCCA = '2'
015400030521     C*
015500030521     C* Come prima cosa verifico se il record relativo sul TITAS ha numero distinta <> da 9999999
015600030521     C     KEYtas30cP    CHAIN     TITAS30C
015700030521 IF 2C                   IF        %found(TITAS30C)
015800030521 IF 3C                   IF        tasNDC <> 9999999
015900030409     C*
016000030409     C* ...quindi verifico il tipo di evento da assegnare alla spedizione
016100030410     C* (tengo l'ultimo evento di tipo giacenza)
016200030411     C                   CLEAR                   depCEV
016300030430     C                   CLEAR                   depCEV_2
016400030411     C                   CLEAR                   wPriorita
016500030411     C                   CLEAR                   wDepPriorita
016600030409     C     KEYevb05P     SETLL     fnevb05l
016700030409     C     KEYevb05P     READE     fnevb05l
016800030430     C                   EVAL      depCEV_2 = evbCEV
016900030409     C                   DOW       not %eof(fnevb05l)
017000030410     C     evbCEV        LOOKUP    sk2Agiac                               19
017100030410     C                   IF        *IN19 = *ON
017200030411     C                   IF        evbCEV = 'G'
017300030411     C                   EVAL      wPriorita = 0
017400030411     C                   ELSE
017500030411     C                   EVAL      wPriorita = 1
017600030411     C                   ENDIF
017700030411     C* Considero l'evento 'G  ' secondario rispetto a tutti gli altri
017800030411     C                   IF        wPriorita >= wDepPriorita
017900030411     C                   EVAL      wDepPriorita = wPriorita
018000030410     C                   EVAL      depCEV = evbCEV
018100030410     C                   ENDIF
018200030411     C*
018300030411     C                   ENDIF
018400030410     C     KEYevb05P     READE     fnevb05l
018500030410     C                   ENDDO
018600030410     C*
018700030410     C* ...distinguo quindi l'evento x identificare quale file scrivere
018800030430     C                   IF        depCEV <> *blanks
018900030410     C     depCEV        LOOKUP    sk2Ainderr                             20
019000030409     C                   IF        *IN20 = *ON
019100030410     C* => Indirizzi errati (file TELE2XLS1)
019200030410     C                   EXSR      TRADXLS1
019300030410     C                   ELSE
019400030410     C     depCEV        LOOKUP    sk2Arif                                21
019500030410     C                   IF        *IN21 = *ON
019600030410     C* => Rifiuti destinatario (file TELE2XLS2)
019700030410     C                   EXSR      TRADXLS2
019800030410     C                   ELSE
019900030410     C* => Altri motivi di reso (file TELE2XLS3)
020000030410     C                   EXSR      TRADXLS3
020100030410     C                   ENDIF
020200030410     C                   ENDIF
020300030430     C                   ELSE
020400030430     C* => Altri motivi di reso (file TELE2XLS3)
020500030430     C                   EXSR      TRADXLS3
020600030430     C                   ENDIF
020700030521 X  3C                   ELSE
020800030521     C                   WRITE     FNVACsss
020900030521     C*
021000030521     C     wVACRRN       chain     fnvac00t
021100030521     C                   if        %found(fnvac00t)
021200030521     C                   delete    FNVACTTT
021300030521     C                   endif
021400030521     C*
021500030521 END3C                   ENDIF
021600030521 END2C                   ENDIF
021700030410     C*
021800030410 X  1C                   ELSE
021900030409     C*
022000030409     C* X le spedizioni OK...
022100030521 IF 4C                   IF        vacDCM > *zeros  AND
022200030410     C                             vacCCA = *blanks
022300030409     C                   EXSR      TRADXLS4
022400030428     C*
022500030409     C                   WRITE     FNVACsss
022600030521     C*
022700030521     C     wVACRRN       chain     fnvac00t
022800030521     C                   if        %found(fnvac00t)
022900030521     C                   delete    FNVACTTT
023000030521     C                   endif
023100030409     C*
023200030521 X  4C                   ELSE
023300030409     C                   WRITE     FNVACsss
023400030521     C*
023500030521     C     wVACRRN       chain     fnvac00t
023600030521     C                   if        %found(fnvac00t)
023700030521     C                   delete    FNVACTTT
023800030521     C                   endif
023900030521     C*
024000030521 END4C                   ENDIF
024100030409 END1C                   ENDIF
024200030603     C                   ENDIF
024300030521     C*
024400030521 X  0C                   ELSE
024500030521     C                   WRITE     FNVACsss
024600030521     C*
024700030521     C     wVACRRN       chain     fnvac00t
024800030521     C                   if        %found(fnvac00t)
024900030521     C                   delete    FNVACTTT
025000030521     C                   endif
025100030521     C*
025200030521 END0C                   ENDIF
025300991027     C*
025400030521     C/EXEC SQL
025500030521     C+ Fetch C1 into :dsvac, :wVACRRN
025600030521     C/END-EXEC
025700030325     C                   ENDDO
025800991027     C*
025900910830     C                   ENDSR
026000030409
026100030409
026200030409
026300030409
026400030409     C     TRADXLS1      BEGSR
026500030409     C*
026600030409     C* Qualora nn ancora fatto imposto la prima riga con le intestazioni dei campi
026700030409     C                   IF        flgIntXLS1 = '0'
026800030430     C                   EVAL      T21XDTA = '"NUMERO ORDINE"'+','+
026900030430     C                                       '"CODICE CLIENTE TELE2"'+','+
027000030430     C                                       '"RAGIONE SOCIALE"'+','+
027100030430     C                                       '"INDIRIZZO"'+','+
027200030409     C                                       '"LOCALITA"'+','+
027300030409     C                                       '"PROVINCIA"'+','+
027400030409     C                                       '"CAP"'+','+
027500030409     C                                       '"DATA RESO"'+','+
027600030409     C                                       '"IMPORTO CONTRASSEGNO"'+','+
027700030507     C                                       '"DESCRIZIONE CAUSALE RESO"'+','+
027800030715     C                                       '"TELEFONO"'+','+
027900030715     C                                       '"DATA SPEDIZIONE"'
028000030409     C                   WRITE     TELE2X01
028100030409     C                   EVAL      flgIntXLS1 = '1'
028200030409     C                   ENDIF
028300030409     C*
028400030409     C* "Chaino" la bolla x reperire i dati mancanti
028500030409     C     KEYtas30cP    CHAIN     TITAS30C
028600030409     C                   IF        %found(TITAS30C)
028700030409     C*
028800030409     C* Reperisco la descrizioine dell'evento che caratterizza la spedizione
028900030409     C                   CLEAR                   ds2A
029000030409     C                   EVAL      tblKUT = 1
029100030409     C                   EVAL      tblCOD = '2A'
029200030409     C                   EVAL      tblKEY = depCEV
029300030409     C     KEYtab        CHAIN     tabel00f
029400030409     C                   IF        %found(TABEL00F)
029500030409     C                   EVAL      ds2A = tblUNI
029600030409     C                   ENDIF
029700030409     C*
029800030409     C* Scrivo i record di dettaglio esiti
029900030430     C                   EVAL      T21XDTA = %trim(%editc(VACRMN:'Q'))+','+
030000030430     C                                          '"'+%trim(VACRMA)+'"'+','+
030100030430     C                                          '"'+%trim(VACRSD)+'"'+','+
030200030430     C                                          '"'+%trim(TASIND)+'"'+','+
030300030409     C                                          '"'+%trim(TASLOD)+'"'+','+
030400030409     C                                          '"'+%trim(TASPRD)+'"'+','+
030500030409     C                                          '"'+%trim(TASCAD)+'"'+','+
030600030409     C                                       %trim(%editc(VACDCM:'Q'))+','+
030700030430     C                                       %trim(%editc(VACCAS:'P'))+','+
030800030507     C                                          '"'+%trim(§2ADES)+'"'+','+
030900030715     C                                          '"'+%trim(VACRMO)+'"'+','+
031000030715     C                                       %trim(VACDSP)
031100030409     C*
031200030409     C                   WRITE     TELE2X01
031300030409     C                   WRITE     FNVACsss
031400030521     C*
031500030521     C     wVACRRN       chain     fnvac00t
031600030521     C                   if        %found(fnvac00t)
031700030521     C                   delete    FNVACTTT
031800030521     C                   endif
031900030409     C*
032000030409     C                   ENDIF
032100030409     C*
032200030409     C                   ENDSR
032300030409
032400030409
032500030409
032600030409     C     TRADXLS2      BEGSR
032700030409     C*
032800030409     C* Qualora nn ancora fatto imposto la prima riga con le intestazioni dei campi
032900030409     C                   IF        flgIntXLS2 = '0'
033000030409     C                   EVAL      T22XDTA = '"NUMERO ORDINE"'+','+
033100030410     C                                       '"CODICE CLIENTE TELE2"'+','+
033200030409     C                                       '"RAGIONE SOCIALE"'+','+
033300030409     C                                       '"INDIRIZZO"'+','+
033400030409     C                                       '"LOCALITA"'+','+
033500030409     C                                       '"PROVINCIA"'+','+
033600030409     C                                       '"CAP"'+','+
033700030409     C                                       '"DATA RESO"'+','+
033800030409     C                                       '"IMPORTO CONTRASSEGNO"'+','+
033900030507     C                                       '"DESCRIZIONE CAUSALE RESO"'+','+
034000030715     C                                       '"TELEFONO"'+','+
034100030715     C                                       '"DATA SPEDIZIONE"'
034200030409     C                   WRITE     TELE2X02
034300030409     C                   EVAL      flgIntXLS2 = '1'
034400030409     C                   ENDIF
034500030409     C*
034600030409     C* "Chaino" la bolla x reperire i dati mancanti
034700030409     C     KEYtas30cP    CHAIN     TITAS30C
034800030409     C                   IF        %found(TITAS30C)
034900030409     C*
035000030409     C* Reperisco la descrizioine dell'evento che caratterizza la spedizione
035100030409     C                   CLEAR                   ds2A
035200030409     C                   EVAL      tblKUT = 1
035300030409     C                   EVAL      tblCOD = '2A'
035400030409     C                   EVAL      tblKEY = depCEV
035500030409     C     KEYtab        CHAIN     tabel00f
035600030409     C                   IF        %found(TABEL00F)
035700030409     C                   EVAL      ds2A = tblUNI
035800030409     C                   ENDIF
035900030409     C*
036000030409     C* Scrivo i record di dettaglio esiti
036100030409     C                   EVAL      T22XDTA = %trim(%editc(VACRMN:'Q'))+','+
036200030409     C                                          '"'+%trim(VACRMA)+'"'+','+
036300030409     C                                          '"'+%trim(VACRSD)+'"'+','+
036400030409     C                                          '"'+%trim(TASIND)+'"'+','+
036500030409     C                                          '"'+%trim(TASLOD)+'"'+','+
036600030409     C                                          '"'+%trim(TASPRD)+'"'+','+
036700030409     C                                          '"'+%trim(TASCAD)+'"'+','+
036800030409     C                                       %trim(%editc(VACDCM:'Q'))+','+
036900030430     C                                       %trim(%editc(VACCAS:'P'))+','+
037000030507     C                                          '"'+%trim(§2ADES)+'"'+','+
037100030715     C                                          '"'+%trim(VACRMO)+'"'+','+
037200030715     C                                       %trim(VACDSP)
037300030409     C*
037400030409     C                   WRITE     TELE2X02
037500030409     C                   WRITE     FNVACsss
037600030521     C*
037700030521     C     wVACRRN       chain     fnvac00t
037800030521     C                   if        %found(fnvac00t)
037900030521     C                   delete    FNVACTTT
038000030521     C                   endif
038100030409     C*
038200030409     C                   ENDIF
038300030409     C*
038400030409     C                   ENDSR
038500030409
038600030409
038700030409
038800030409     C     TRADXLS3      BEGSR
038900030409     C*
039000030409     C* Qualora nn ancora fatto imposto la prima riga con le intestazioni dei campi
039100030409     C                   IF        flgIntXLS3 = '0'
039200030409     C                   EVAL      T23XDTA = '"NUMERO ORDINE"'+','+
039300030410     C                                       '"CODICE CLIENTE TELE2"'+','+
039400030409     C                                       '"RAGIONE SOCIALE"'+','+
039500030409     C                                       '"INDIRIZZO"'+','+
039600030409     C                                       '"LOCALITA"'+','+
039700030409     C                                       '"PROVINCIA"'+','+
039800030409     C                                       '"CAP"'+','+
039900030409     C                                       '"DATA RESO"'+','+
040000030409     C                                       '"IMPORTO CONTRASSEGNO"'+','+
040100030507     C                                       '"DESCRIZIONE CAUSALE RESO"'+','+
040200030715     C                                       '"TELEFONO"'+','+
040300030715     C                                       '"DATA SPEDIZIONE"'
040400030409     C                   WRITE     TELE2X03
040500030409     C                   EVAL      flgIntXLS3 = '1'
040600030409     C                   ENDIF
040700030409     C*
040800030409     C* "Chaino" la bolla x reperire i dati mancanti
040900030409     C     KEYtas30cP    CHAIN     TITAS30C
041000030409     C                   IF        %found(TITAS30C)
041100030409     C*
041200030409     C* Reperisco la descrizioine dell'evento che caratterizza la spedizione
041300030409     C                   CLEAR                   ds2A
041400030409     C                   EVAL      tblKUT = 1
041500030409     C                   EVAL      tblCOD = '2A'
041600030430     C                   IF        depCEV <> *blanks
041700030409     C                   EVAL      tblKEY = depCEV
041800030430     C                   ELSE
041900030430     C                   EVAL      tblKEY = depCEV_2
042000030430     C                   ENDIF
042100030409     C     KEYtab        CHAIN     tabel00f
042200030409     C                   IF        %found(TABEL00F)
042300030409     C                   EVAL      ds2A = tblUNI
042400030430     C                   IF        §2AFTC = 'A'
042500030430     C                   CLEAR                   ds2A
042600030430     C                   EVAL      tblKUT = 1
042700030430     C                   EVAL      tblCOD = '2A'
042800030430     C                   EVAL      tblKEY = '017'
042900030430     C     KEYtab        CHAIN     tabel00f
043000030430     C                   IF        %found(TABEL00F)
043100030430     C                   EVAL      ds2A = tblUNI
043200030430     C                   ENDIF
043300030430     C                   ENDIF
043400030409     C                   ENDIF
043500030409     C*
043600030409     C* Scrivo i record di dettaglio esiti
043700030409     C                   EVAL      T23XDTA = %trim(%editc(VACRMN:'Q'))+','+
043800030409     C                                          '"'+%trim(VACRMA)+'"'+','+
043900030409     C                                          '"'+%trim(VACRSD)+'"'+','+
044000030409     C                                          '"'+%trim(TASIND)+'"'+','+
044100030409     C                                          '"'+%trim(TASLOD)+'"'+','+
044200030409     C                                          '"'+%trim(TASPRD)+'"'+','+
044300030409     C                                          '"'+%trim(TASCAD)+'"'+','+
044400030409     C                                       %trim(%editc(VACDCM:'Q'))+','+
044500030430     C                                       %trim(%editc(VACCAS:'P'))+','+
044600030507     C                                          '"'+%trim(§2ADES)+'"'+','+
044700030715     C                                          '"'+%trim(VACRMO)+'"'+','+
044800030715     C                                       %trim(VACDSP)
044900030409     C*
045000030409     C                   WRITE     TELE2X03
045100030409     C                   WRITE     FNVACsss
045200030521     C*
045300030521     C     wVACRRN       chain     fnvac00t
045400030521     C                   if        %found(fnvac00t)
045500030521     C                   delete    FNVACTTT
045600030521     C                   endif
045700030409     C*
045800030409     C                   ENDIF
045900030409     C*
046000030409     C                   ENDSR
046100030409
046200030409
046300030409
046400030409     C     TRADXLS4      BEGSR
046500030409     C*
046600030409     C* Qualora nn ancora fatto imposto la prima riga con le intestazioni dei campi
046700030409     C                   IF        flgIntXLS4 = '0'
046800030409     C                   EVAL      T24XDTA = '"NUMERO ORDINE"'+','+
046900030410     C                                       '"CODICE CLIENTE TELE2"'+','+
047000030409     C                                       '"RAGIONE SOCIALE"'+','+
047100030409     C                                       '"INDIRIZZO"'+','+
047200030409     C                                       '"LOCALITA"'+','+
047300030409     C                                       '"PROVINCIA"'+','+
047400030409     C                                       '"CAP"'+','+
047500030411     C                                       '"DATA CONSEGNA"'+','+
047600030715     C                                       '"IMPORTO CONTRASSEGNO"'+','+
047700030715     C                                       '"DATA SPEDIZIONE"'
047800030409     C                   WRITE     TELE2X04
047900030409     C                   EVAL      flgIntXLS4 = '1'
048000030409     C                   ENDIF
048100030409     C*
048200030409     C* "Chaino" la bolla x reperire i dati mancanti
048300030409     C     KEYtas30cP    CHAIN     TITAS30C
048400030409     C                   IF        %found(TITAS30C)
048500030409     C*
048600030409     C* Scrivo i record di dettaglio esiti
048700030409     C                   EVAL      T24XDTA = %trim(%editc(VACRMN:'Q'))+','+
048800030409     C                                          '"'+%trim(VACRMA)+'"'+','+
048900030409     C                                          '"'+%trim(VACRSD)+'"'+','+
049000030409     C                                          '"'+%trim(TASIND)+'"'+','+
049100030409     C                                          '"'+%trim(TASLOD)+'"'+','+
049200030409     C                                          '"'+%trim(TASPRD)+'"'+','+
049300030409     C                                          '"'+%trim(TASCAD)+'"'+','+
049400030409     C                                       %trim(%editc(VACDCM:'Q'))+','+
049500030715     C                                       %trim(%editc(VACCAS:'P'))+','+
049600030715     C                                       %trim(VACDSP)
049700030409     C*
049800030409     C                   WRITE     TELE2X04
049900030409     C*
050000030409     C                   ENDIF
050100030409     C*
050200030409     C                   ENDSR
050300030526
050400030526
050500030526
050600030526      /TITLE Valorizzazione Progressivo Applicazione
050700030526     C     calprog       begsr
050800030526     C*
050900030526     C                   open      tis7prgf
051000030526     C*
051100030526     C                   read(e)   tis7prgf
051200030526     C                   if        not %error
051300030526     C                   eval      dwlprg = f_tis7prgf
051400030526     C*
051500030526     C                   move(p)   dwlprg        wrkprg
051600030526     C                   add       1             wrkprg
051700030526     C                   move(p)   wrkprg        dwlprg
051800030526     C                   movel     'OE'          dwlprg
051900030526     C*
052000030526     C                   eval      f_tis7prgf = dwlprg
052100030526     C                   update    tis7prg0
052200030526     C                   endif
052300030526     C*
052400030526     C                   close     tis7prgf
052500030526     C*
052600030526     C                   endsr
052700030526
052800030526
052900030410
053000030410
053100030410
053200030526     C     CREALOGS      BEGSR
053300030519     C*
053400030526     C* Valorizzo i campi comuni a tutti i reord di log da "clonare"
053500030526     C                   CLEAR                   TIVLT000
053600030526     C                   EVAL      vltKSC = '0' + prmksc
053700030526     C                   EVAL      vltKSU = '0' + prmksc
053800030526     C                   EVAL      vltMBD = 'M0'+ prmksc
053900030526     C                   EVAL      vltSND = 'D'
054000030526     C                   EVAL      vltFLS = 'FNVAC00S'
054100030526     C*
054200030526     C                   time                    wrkoggiiso
054300030526     C                   extrct    wrkoggiiso:*m wrkmese
054400030526     C                   extrct    wrkoggiiso:*d wrkday
054500030526     C                   eval      vltMBS =
054600030526     C                             mc(wrkmese)
054700030526     C                             + wrkdaychr
054800030526     C                             + %subst(vltKSU:2:7)
054900030526     C*
055000030526     C                   EVAL      vltDAT = DATCOR
055100030526     C                   EVAL      vltORA = ORACOR
055200030526     C                   EVAL      vltISV = 'OE'
055300030526     C                   EVAL      vltTIA = 'F'
055400030526     C                   EVAL      vltFL2 = '01'
055500030526     C                   EVAL      vltSTS = '0'
055600030410     C*
055700030410     C* => scrivo record di log x tipo file 2T (TELE2XLS2)
055800030526     C*
055900030526     C                   exsr      calprog
056000030526     C                   EVAL      vltPRG = dwlprg
056100030526     C*
056200030410     C                   EVAL      vltTIP = '2T'
056300030410     C                   EVAL      vltFLD = 'TELE2XLS2'
056400030410     C                   WRITE     TIVLT000
056500030410     C* => scrivo record di log x tipo file 3T (TELE2XLS3)
056600030526     C*
056700030526     C                   exsr      calprog
056800030526     C                   EVAL      vltPRG = dwlprg
056900030526     C*
057000030410     C                   EVAL      vltTIP = '3T'
057100030410     C                   EVAL      vltFLD = 'TELE2XLS3'
057200030410     C                   WRITE     TIVLT000
057300030410     C* => scrivo record di log x tipo file 4T (TELE2XLS4)
057400030526     C*
057500030526     C                   exsr      calprog
057600030526     C                   EVAL      vltPRG = dwlprg
057700030526     C*
057800030410     C                   EVAL      vltTIP = '4T'
057900030410     C                   EVAL      vltFLD = 'TELE2XLS4'
058000030410     C                   WRITE     TIVLT000
058100030410     C*
058200030410     C                   ENDSR
058300030410
058400030410
058500030410
058600030410     C     CARTAB        BEGSR
058700030410     C*
058800030410     C                   clear                   sk2Agiac
058900030410     C                   z-add     *zeros        i                 3 0
059000030410     C                   eval      tblKUT = 1
059100030410     C                   eval      tblCOD = '2A'
059200030410     C     KEYtabP       setll     tabel00f
059300030410     C     KEYtabP       reade     tabel00f
059400030410     C                   dow       not %eof(tabel00f)
059500030410     C                   movel(p)  tblUNI        ds2a
059600030410     C                   if        §2AFTC = 'G'
059700030410     C                   add       1             i
059800030410     C                   eval      sk2Agiac(i) = tblKEY
059900030410     C                   endif
060000030410     C     KEYtabP       reade     tabel00f
060100030410     C                   enddo
060200030410     C*
060300030410     C                   ENDSR
060400030417
060500030417
060600030417
060700030417     C*----------------------------------------------------*
060800030417     C*  CONTROLLO NUMERICITA' CAMPI
060900030417     C*----------------------------------------------------*
061000030417     C     CHKNUM        BEGSR
061100030417     C*
061200030417     C                   call(e)   'ISNUMERIC'
061300030417     C                   PARM                    PiStr            30
061400030417     C                   PARM      ','           PiDecChr          1
061500030417     C                   PARM      *ZEROS        PiVal            30 9
061600030417     C                   PARM      '0'           PiInt             1
061700030417     C                   PARM      '0'           PiNum             1
061800030417     C                   IF        %error
061900030417     C                   EVAL      PiInt=*off
062000030417     C                   EVAL      PiNum=*off
062100030417     C                   ENDIF
062200030417     C*
062300030417     C                   ENDSR
062400030417     C***
062500030409
062600030410
062700030409
062800991027
062900991027      /TITLE Operazioni iniziali.
063000991027     C     *inzsr        BEGSR
063100991027     C*
063200991027     C     *ENTRY        PLIST
063300030409     C                   PARM                    prmppt
063400030409     C     wrkesito      PARM      wrkesito      prmesito
063500030521     C                   PARM                    prmksc
063600030409     C*
063700030409     C* IMPOSTA LA DATA CORRENTE
063800030409     C                   TIME                    N14              14 0
063900030409     C                   MOVE      N14           N8                8 0
064000030526     C                   MOVEL     N14           ORACOR            6 0
064100030409     C                   Z-ADD     N8            G08DAT
064200030409     C                   Z-ADD     *ZEROS        G08INV
064300030409     C                   MOVEL     '0'           G08ERR
064400030409     C                   CALL      'XSRDA8'
064500030409     C                   PARM                    WLBDA8
064600030409     C                   Z-ADD     G08INV        DATCOR            8 0
064700030410     C*
064800030410     C* CARICO LE TABELLE OCCORRENTI
064900030410     C                   EXSR      CARTAB
065000030409     C****
065100030409     C* Definizione chiavi
065200030409     C****
065300030410     C* Chiave su TABEL00F - parziale
065400030410     C     KEYtabP       KLIST
065500030410     C                   KFLD                    tblKUT
065600030410     C                   KFLD                    tblCOD
065700030409     C* Chiave su TABEL00F - completa
065800030409     C     KEYtab        KLIST
065900030409     C                   KFLD                    tblKUT
066000030409     C                   KFLD                    tblCOD
066100030409     C                   KFLD                    tblKEY
066200030409     C* Chiave su FNEVB05L - parziale
066300030409     C     KEYevb05P     KLIST
066400030409     C                   KFLD                    vacAAS
066500030409     C                   KFLD                    vacLNP
066600030409     C                   KFLD                    vacNRS
066700030409     C                   KFLD                    vacNSP
066800030409     C* Chiave su TITAS30C - parziale
066900030409     C     KEYtas30cP    KLIST
067000030409     C                   KFLD                    vacAAS
067100030409     C                   KFLD                    vacLNP
067200030409     C                   KFLD                    vacNRS
067300030409     C                   KFLD                    vacNSP
067400030603     C* Chiave su FNLBL01L - completa
067500030603     C     KEYlbl01C     KLIST
067600030603     C                   KFLD                    vacAAS
067700030603     C                   KFLD                    vacLNP
067800030603     C                   KFLD                    vacNRS
067900030603     C                   KFLD                    vacNSP
068000030409     C* Chiave su TIVIR01L - completa
068100030409     C     KEYvir01      KLIST
068200030409     C                   KFLD                    virKSC
068300030409     C                   KFLD                    virSEQ
068400991027     C*
068500991027     C                   ENDSR
068600030409     C*------------------------------------------------------------
068700030409** sk2Ainderr - SCHIERA CODICI EVENTO X INDIRIZZO ERRATO
068800030409019
068900030409020
069000030409021
069100030409022
069200030409
069300030409** sk2Arif    - SCHIERA CODICI EVENTO X RIFIUTO DESTINATARIO
069400030409001
069500030409002
069600030409003
069700030409004
069800030409005
069900030409007
070000030409008
070100030409010
070200030409012
070300030409013
070400030409014
070500030409027
070600030409029
070700030409030
070800030409031
070900030409RFM
071000030409RFD
071100030409
071200030409
071300030409
071400030526** ctdata mc
071500030526ABCDEFGHILMN
