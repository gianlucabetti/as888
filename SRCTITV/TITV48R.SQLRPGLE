000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200991027     H dftactgrp(*yes)
000300991027
000400030521     FFNVAC00T  uF   E             DISK    RENAME(FNVAC000:FNVACTTT)
000500030911     FTELE2XLS1 uF A E             DISK
000600030911     FTELE2XLS2 uF A E             DISK
000700030911     FTELE2XLS3 uF A E             DISK
000800030911     FTELE2XLS4 uF A E             DISK
000900030331     FFNVAC00S  O    E             DISK    rename(fnvac000:fnvacsss)
001000030812     FFNVACWWT  O    E             DISK
001100030409     FTABEL00F  IF   E           K DISK
001200030409     FTITAS30C  IF   E           K DISK
001300030603     FFNLBL01L  IF   E           K DISK
001400030409     FFNEVB05L  IF   E           K DISK
001500030409     FTIVIR01L  UF   E           K DISK
001600030411     FTIVLT01L  UF A E           K DISK
001700030526     Ftis7prgf  uf   e             disk    RENAME(tis7prgf:tis7prg0)
001800030526     F                                     PREFIX(f_)
001900030526     F                                     USROPN
002000991027
002100991027     D prmppt          s             50
002200991027     D prmesito        s              1
002300030521     D prmksc          s              7
002400030813     D prmksc2         s              7
002500991027     D wrkesito        s                   like(prmesito)
002600030409     D flgIntXLS1      s              1    inz('0')
002700030409     D flgIntXLS2      s              1    inz('0')
002800030409     D flgIntXLS3      s              1    inz('0')
002900030409     D flgIntXLS4      s              1    inz('0')
003000030911     D wInt1           s              1    inz('N')
003100030911     D wInt2           s              1    inz('N')
003200030911     D wInt3           s              1    inz('N')
003300030911     D wInt4           s              1    inz('N')
003400030409     D depCEV          s                   like(evbCEV)
003500030430     D depCEV_2        s                   like(evbCEV)
003600030411     D wPriorita       s              1  0 inz(*zeros)
003700030411     D wDepPriorita    s              1  0 inz(*zeros)
003800030417     D wParmGgSet      s              1  0 inz(*zeros)
003900030521     D wVACRRN         s             15  0 inz(*zeros)
004000030526     D wrkoggiiso      s               d
004100030526     D wrkmese         s              2  0
004200030526     D dwlprg          s             10    INZ(*all'0')
004300030526     D wrkprg          s              8  0 INZ(*zeros)
004400030609     D wData1_iso      s               d
004500030609     D wData2_iso      s               d
004600030609     D wData2_alfa     s              8
004700030609     D wData2_num      s              8  0 INZ(*zeros)
004800030609     D wNumGg          s              3  0 INZ(*zeros)
004900030812     D wISV            s              2    INZ(*blanks)
005000030526     D                 ds                  INZ
005100030526     D wrkday                  1      2s 0
005200030526     D wrkdaychr                      2    overlay(wrkday)
005300030325
005400030409     D dsvac         e ds                  extname(fnvac00t)
005500030409     D ds2a          e ds
005600030409     D X16GGSETDS    e ds
005700030409     D sk2Ainderr      s              3    dim(5) ctdata perrcd(1)
005800030409     D sk2Arif         s              3    dim(20) ctdata perrcd(1)
005900080903     D sk2Agiac        s              3    dim(200)
006000030526     D mc              s              1    dim(12) ctdata perrcd(12)
006100030409
006200030409     D*---------------------
006300030409     D* DS "XSRDA8" - CONTROLLA DATA (8)
006400030409     D*---------------------
006500030409     D WLBDA8          DS
006600030409     D  G08DAT                 1      8  0
006700030409     D  G08INV                 9     16  0
006800030409     D  G08ERR                17     17
006900030409     D  G08TGI                18     22  0
007000030409
007100030812
007200030812     D*---------------------
007300030812     D* DS PER EDITARE I CAMPI NUMERICI IN ASCII
007400030812     D*---------------------
007500030812     D* 10+3
007600030812     D                 DS
007700030812     D  WS15                   1      1
007800030812     D  WI15                   2     11
007900030812     D  WV15                  12     12    inz(',')
008000030812     D  WD15                  13     15
008100030812     D  WASC15                 1     15
008200030812     D*  6+1
008300030812     D                 DS
008400030812     D  WS09                   1      1
008500030812     D  WI09                   2      7
008600030812     D  WV09                   8      8    inz(',')
008700030812     D  WD09                   9      9
008800030812     D  WASC09                 1      9
008900030812     D*  2+3
009000030812     D                 DS
009100030812     D  WS07                   1      1
009200030812     D  WI07                   2      3
009300030812     D  WV07                   4      4    inz(',')
009400030812     D  WD07                   5      7
009500030812     D  WASC07                 1      7
009600030409
009700030812      /TITLE MAIN
009800030812     C*
009900030812     C* Se trattasi di cliente 0052454 effettuo le considerazioni qui sotto, se trattasi invece
010000040223     C* di cliente 0052552 invece elaboro sempre cmq
010100030812     C                   IF        prmksc = '0052454'
010200030417     C*
010300030812     C* Verifico se ricevuti dei parametri (nella fatispecie il giorno della settimana in cui
010400030417     C* effettuare la traduzione
010500030417     C                   EVAL      wParmGgSet = 2                               * DFT = Lunedì
010600030417     C                   IF        %subst(prmppt:9:1) <> *blanks
010700030417     C                   EVAL      PiStr=%trim(%subst(prmppt:9:1))
010800030417     C                   EXSR      CHKNUM
010900030417     C                   IF        PiInt=*on
011000030417     C                   Z-ADD     PiVal         wParmGgSet
011100030417     C                   ENDIF
011200030417     C                   ENDIF
011300030409     C*
011400030409     C* Innanzitutto verifico se la traduzione è da eseguire in quanto il cliente vuole i dati
011500030609     C* solamente 1 volta a settimana (nella fattispecie abbiamo stabilito il lunedì)...
011600030609     C* ...oppure se in ogni caso dall'ultima traduzione sono passati + d 7 giorni.
011700030609     C                   EVAL      virKSC = '0' + prmksc
011800030609     C                   EVAL      virSEQ = 001
011900030609     C     KEYvir01      CHAIN     TIVIR01L
012000030609     C                   IF        %found(TIVIR01L)
012100030609     C* - verifico se sono passati + d 7 giorni dall'ultima traduzione
012200030609     C     *ISO          MOVE      DATCOR        wData1_iso
012300030609     C                   EVAL      wData2_alfa = %subst(virPPT:1:8)
012400030609     C                   MOVE      wData2_alfa   wData2_num
012500030609     C     *ISO          MOVE      wData2_num    wData2_iso
012600030609     C                   Z-ADD     *zeros        wNumGg
012700030609     C     wData1_iso    SUBDUR    wData2_iso    wNumGg:*D
012800030609     C* - verifico se trattasi del giorno stabilito
012900030409     C                   CLEAR                   X16GGSETDS
013000030409     C     *ISO          MOVE      DATCOR        X16DATA
013100030409     C                   CALL      'X16GGSET'
013200030409     C                   PARM                    X16GGSETDS
013300030609     C* Se soddisfatte almeno 1 delle 2 condizioni => eseguo traduzione
013400030609     C                   IF        X16GIORNO = wParmGgSet OR
013500030609     C                             %abs(wNumGg) > 7
013600030417     C                   IF        %subst(virPPT:1:8) <>
013700030417     C                             %editw(DATCOR:'        ')
013800030911     C                   EXSR      chkRECFILMBR
013900030409     C                   EXSR      traduci
014000030417     C                   EVAL      %subst(virPPT:1:8) =
014100030417     C                             %editw(DATCOR:'        ')
014200030409     C                   UPDATE    TIVIR000
014300030410     C                   EXSR      crealogs
014400030409     C                   ENDIF
014500030409     C                   ENDIF
014600030409     C                   ENDIF
014700030812     C*
014800030812     C                   ELSE
014900030812     C*
015000030812     C* Forzo x sicurezza il test che si tratti del cliente 0052552
015100030812     C                   IF        prmksc = '0052552'
015200030911     C                   EXSR      chkRECFILMBR
015300030812     C                   EXSR      traduci
015400030812     C                   EXSR      crealogs
015500030812     C                   ENDIF
015600030812     C                   ENDIF
015700030409     C*
015800030409     C                   EVAL      wrkesito = '0'
015900030409     C*
016000921023     C                   SETON                                        LR
016100030409
016200030409
016300030911
016400030911     C     chkRECFILMBR  BEGSR
016500030911     C*
016600030911     C                   READ      TELE2XLS1                              55
016700030911     C                   IF        *IN55 = *ON
016800030911     C                   MOVEL     'S'           wInt1
016900030911     C                   ENDIF
017000030911     C*
017100030911     C                   READ      TELE2XLS2                              55
017200030911     C                   IF        *IN55 = *ON
017300030911     C                   MOVEL     'S'           wInt2
017400030911     C                   ENDIF
017500030911     C*
017600030911     C                   READ      TELE2XLS3                              55
017700030911     C                   IF        *IN55 = *ON
017800030911     C                   MOVEL     'S'           wInt3
017900030911     C                   ENDIF
018000030911     C*
018100030911     C                   READ      TELE2XLS4                              55
018200030911     C                   IF        *IN55 = *ON
018300030911     C                   MOVEL     'S'           wInt4
018400030911     C                   ENDIF
018500030911     C*
018600030911     C                   ENDSR
018700030409
018800030911
018900991027
019000991027     C     traduci       BEGSR
019100991027     C*
019200030521     C/EXEC SQL
019300030521     C+ declare C1 cursor for
019400030521     C+ select fnvac00t.*, rrn(fnvac00t)
019500030521     C+ from fnvac00t order by vacnsp
019600030521     C+ for read only
019700030521     C/END-EXEC
019800030521     C
019900030521     C/EXEC SQL
020000030521     C+ open C1
020100030521     C/END-EXEC
020200030521     C
020300030521     C/EXEC SQL
020400030521     C+ Fetch C1 into :dsvac, :wVACRRN
020500030521     C/END-EXEC
020600030521     C*
020700030521     C                   DOW       sqlcod = *zeros
020800040223     C*
020900030812     C                   MOVEL     'N'           FlgTELE2          1
021000030603     C*
021100030603     C* Testo che la bolla nn sia 1 bolla "figlia"
021200030603     C     KEYlbl01C     CHAIN     FNLBL01L
021300030603     C                   IF        %found(FNLBL01L)
021400030603     C                   ELSE
021500040226     C*
021600040226     C* Se trattasi del cliente SIRTI (0052552) scrivo sempre il VAC
021700040226     C                   IF        prmksc = '0052552'
021800040226     C* Solo le bolle aventi data consegna merce valorizzata escludendo le messe in consegna
021900040226     C                   IF        vacDCM > 0 AND vacCCA <> 'C'
022000040226     C                   EXSR      TRADVAC
022100040226     C                   ENDIF
022200040226     C                   ENDIF
022300030715     C*
022400030715     C* Compongo la data spedizoine come unico campo alfa
022500030715     C                   MOVEL(P)  VACAAS        VACDSP            8
022600030715     C                   MOVE      VACMGS        VACDSP
022700030409     C*
022800030410     C* Innanzitutto elaboro solo le bolle "chiuse" con reso e aventi una apertura giacenza
022900030410 IF 1C                   IF        vacDCM > *zeros AND
023000030429     C                             vacCCA = '2'
023100030521     C*
023200030521     C* Come prima cosa verifico se il record relativo sul TITAS ha numero distinta <> da 9999999
023300030521     C     KEYtas30cP    CHAIN     TITAS30C
023400030521 IF 2C                   IF        %found(TITAS30C)
023500030521 IF 3C                   IF        tasNDC <> 9999999
023600030409     C*
023700030409     C* ...quindi verifico il tipo di evento da assegnare alla spedizione
023800030410     C* (tengo l'ultimo evento di tipo giacenza)
023900030411     C                   CLEAR                   depCEV
024000030430     C                   CLEAR                   depCEV_2
024100030411     C                   CLEAR                   wPriorita
024200030411     C                   CLEAR                   wDepPriorita
024300030409     C     KEYevb05P     SETLL     fnevb05l
024400030409     C     KEYevb05P     READE     fnevb05l
024500030430     C                   EVAL      depCEV_2 = evbCEV
024600030409     C                   DOW       not %eof(fnevb05l)
024700030410     C     evbCEV        LOOKUP    sk2Agiac                               19
024800030410     C                   IF        *IN19 = *ON
024900031006     C                   IF        evbCEV = 'G' OR evbCEV = 'MIC'
025000030411     C                   EVAL      wPriorita = 0
025100030411     C                   ELSE
025200030411     C                   EVAL      wPriorita = 1
025300030411     C                   ENDIF
025400031006     C* Considero gli eventi 'G  ' e 'MIC' secondari rispetto a tutti gli altri
025500030411     C                   IF        wPriorita >= wDepPriorita
025600030411     C                   EVAL      wDepPriorita = wPriorita
025700030410     C                   EVAL      depCEV = evbCEV
025800030410     C                   ENDIF
025900030411     C*
026000030411     C                   ENDIF
026100030410     C     KEYevb05P     READE     fnevb05l
026200030410     C                   ENDDO
026300030410     C*
026400030410     C* ...distinguo quindi l'evento x identificare quale file scrivere
026500030430     C                   IF        depCEV <> *blanks
026600030410     C     depCEV        LOOKUP    sk2Ainderr                             20
026700030409     C                   IF        *IN20 = *ON
026800030410     C* => Indirizzi errati (file TELE2XLS1)
026900030410     C                   EXSR      TRADXLS1
027000030410     C                   ELSE
027100030410     C     depCEV        LOOKUP    sk2Arif                                21
027200030410     C                   IF        *IN21 = *ON
027300030410     C* => Rifiuti destinatario (file TELE2XLS2)
027400030410     C                   EXSR      TRADXLS2
027500030410     C                   ELSE
027600030410     C* => Altri motivi di reso (file TELE2XLS3)
027700030410     C                   EXSR      TRADXLS3
027800030410     C                   ENDIF
027900030410     C                   ENDIF
028000030430     C                   ELSE
028100030430     C* => Altri motivi di reso (file TELE2XLS3)
028200030430     C                   EXSR      TRADXLS3
028300030430     C                   ENDIF
028400030521 X  3C                   ELSE
028500030521     C*
028600030521 END3C                   ENDIF
028700030521 END2C                   ENDIF
028800030410     C*
028900030410 X  1C                   ELSE
029000030409     C*
029100030409     C* X le spedizioni OK...
029200030521 IF 4C                   IF        vacDCM > *zeros  AND
029300030410     C                             vacCCA = *blanks
029400030409     C                   EXSR      TRADXLS4
029500030428     C*
029600030812     C                   WRITE     FNVACsss
029700030521 END4C                   ENDIF
029800030409 END1C                   ENDIF
029900030603     C                   ENDIF
030000040226     C*
030100040226     C* In ogni caso storicizzo il record d input e lo cancello
030200040226     C                   WRITE     FNVACsss
030300040226     C*
030400040226     C     wVACRRN       chain     fnvac00t
030500040226     C                   if        %found(fnvac00t)
030600040226     C                   delete    FNVACTTT
030700040226     C                   endif
030800991027     C*
030900030521     C/EXEC SQL
031000030521     C+ Fetch C1 into :dsvac, :wVACRRN
031100030521     C/END-EXEC
031200030325     C                   ENDDO
031300991027     C*
031400910830     C                   ENDSR
031500030409
031600030409
031700030409
031800030409
031900030409     C     TRADXLS1      BEGSR
032000030409     C*
032100030409     C* Qualora nn ancora fatto imposto la prima riga con le intestazioni dei campi
032200030911     C                   IF        flgIntXLS1 = '0' AND wInt1 = 'S'
032300030430     C                   EVAL      T21XDTA = '"NUMERO ORDINE"'+','+
032400030430     C                                       '"CODICE CLIENTE TELE2"'+','+
032500030430     C                                       '"RAGIONE SOCIALE"'+','+
032600030430     C                                       '"INDIRIZZO"'+','+
032700030409     C                                       '"LOCALITA"'+','+
032800030409     C                                       '"PROVINCIA"'+','+
032900030409     C                                       '"CAP"'+','+
033000030409     C                                       '"DATA RESO"'+','+
033100030409     C                                       '"IMPORTO CONTRASSEGNO"'+','+
033200030507     C                                       '"DESCRIZIONE CAUSALE RESO"'+','+
033300030715     C                                       '"TELEFONO"'+','+
033400030715     C                                       '"DATA SPEDIZIONE"'
033500030409     C                   WRITE     TELE2X01
033600030409     C                   EVAL      flgIntXLS1 = '1'
033700030409     C                   ENDIF
033800030409     C*
033900030409     C* "Chaino" la bolla x reperire i dati mancanti
034000030409     C     KEYtas30cP    CHAIN     TITAS30C
034100030409     C                   IF        %found(TITAS30C)
034200030409     C*
034300030409     C* Reperisco la descrizioine dell'evento che caratterizza la spedizione
034400030409     C                   CLEAR                   ds2A
034500030409     C                   EVAL      tblKUT = 1
034600030409     C                   EVAL      tblCOD = '2A'
034700030409     C                   EVAL      tblKEY = depCEV
034800030409     C     KEYtab        CHAIN     tabel00f
034900030409     C                   IF        %found(TABEL00F)
035000030409     C                   EVAL      ds2A = tblUNI
035100030409     C                   ENDIF
035200030409     C*
035300030409     C* Scrivo i record di dettaglio esiti
035400030430     C                   EVAL      T21XDTA = %trim(%editc(VACRMN:'Q'))+','+
035500030430     C                                          '"'+%trim(VACRMA)+'"'+','+
035600030430     C                                          '"'+%trim(VACRSD)+'"'+','+
035700030430     C                                          '"'+%trim(TASIND)+'"'+','+
035800030409     C                                          '"'+%trim(TASLOD)+'"'+','+
035900030409     C                                          '"'+%trim(TASPRD)+'"'+','+
036000030409     C                                          '"'+%trim(TASCAD)+'"'+','+
036100030409     C                                       %trim(%editc(VACDCM:'Q'))+','+
036200030430     C                                       %trim(%editc(VACCAS:'P'))+','+
036300030507     C                                          '"'+%trim(§2ADES)+'"'+','+
036400030715     C                                          '"'+%trim(VACRMO)+'"'+','+
036500030715     C                                       %trim(VACDSP)
036600030409     C*
036700030812     C                   MOVEL     'S'           FlgTELE2
036800030409     C                   WRITE     TELE2X01
036900030409     C                   WRITE     FNVACsss
037000030521     C*
037100030521     C     wVACRRN       chain     fnvac00t
037200030521     C                   if        %found(fnvac00t)
037300030521     C                   delete    FNVACTTT
037400030521     C                   endif
037500030409     C*
037600030409     C                   ENDIF
037700030409     C*
037800030409     C                   ENDSR
037900030409
038000030409
038100030409
038200030409     C     TRADXLS2      BEGSR
038300030409     C*
038400030409     C* Qualora nn ancora fatto imposto la prima riga con le intestazioni dei campi
038500030911     C                   IF        flgIntXLS2 = '0' AND wInt2 = 'S'
038600030409     C                   EVAL      T22XDTA = '"NUMERO ORDINE"'+','+
038700030410     C                                       '"CODICE CLIENTE TELE2"'+','+
038800030409     C                                       '"RAGIONE SOCIALE"'+','+
038900030409     C                                       '"INDIRIZZO"'+','+
039000030409     C                                       '"LOCALITA"'+','+
039100030409     C                                       '"PROVINCIA"'+','+
039200030409     C                                       '"CAP"'+','+
039300030409     C                                       '"DATA RESO"'+','+
039400030409     C                                       '"IMPORTO CONTRASSEGNO"'+','+
039500030507     C                                       '"DESCRIZIONE CAUSALE RESO"'+','+
039600030715     C                                       '"TELEFONO"'+','+
039700030715     C                                       '"DATA SPEDIZIONE"'
039800030409     C                   WRITE     TELE2X02
039900030409     C                   EVAL      flgIntXLS2 = '1'
040000030409     C                   ENDIF
040100030409     C*
040200030409     C* "Chaino" la bolla x reperire i dati mancanti
040300030409     C     KEYtas30cP    CHAIN     TITAS30C
040400030409     C                   IF        %found(TITAS30C)
040500030409     C*
040600030409     C* Reperisco la descrizioine dell'evento che caratterizza la spedizione
040700030409     C                   CLEAR                   ds2A
040800030409     C                   EVAL      tblKUT = 1
040900030409     C                   EVAL      tblCOD = '2A'
041000030409     C                   EVAL      tblKEY = depCEV
041100030409     C     KEYtab        CHAIN     tabel00f
041200030409     C                   IF        %found(TABEL00F)
041300030409     C                   EVAL      ds2A = tblUNI
041400030409     C                   ENDIF
041500030409     C*
041600030409     C* Scrivo i record di dettaglio esiti
041700030409     C                   EVAL      T22XDTA = %trim(%editc(VACRMN:'Q'))+','+
041800030409     C                                          '"'+%trim(VACRMA)+'"'+','+
041900030409     C                                          '"'+%trim(VACRSD)+'"'+','+
042000030409     C                                          '"'+%trim(TASIND)+'"'+','+
042100030409     C                                          '"'+%trim(TASLOD)+'"'+','+
042200030409     C                                          '"'+%trim(TASPRD)+'"'+','+
042300030409     C                                          '"'+%trim(TASCAD)+'"'+','+
042400030409     C                                       %trim(%editc(VACDCM:'Q'))+','+
042500030430     C                                       %trim(%editc(VACCAS:'P'))+','+
042600030507     C                                          '"'+%trim(§2ADES)+'"'+','+
042700030715     C                                          '"'+%trim(VACRMO)+'"'+','+
042800030715     C                                       %trim(VACDSP)
042900030409     C*
043000030812     C                   MOVEL     'S'           FlgTELE2
043100030409     C                   WRITE     TELE2X02
043200030409     C                   WRITE     FNVACsss
043300030521     C*
043400030521     C     wVACRRN       chain     fnvac00t
043500030521     C                   if        %found(fnvac00t)
043600030521     C                   delete    FNVACTTT
043700030521     C                   endif
043800030409     C*
043900030409     C                   ENDIF
044000030409     C*
044100030409     C                   ENDSR
044200030409
044300030409
044400030409
044500030409     C     TRADXLS3      BEGSR
044600030409     C*
044700030409     C* Qualora nn ancora fatto imposto la prima riga con le intestazioni dei campi
044800030911     C                   IF        flgIntXLS3 = '0' AND wInt3 = 'S'
044900030409     C                   EVAL      T23XDTA = '"NUMERO ORDINE"'+','+
045000030410     C                                       '"CODICE CLIENTE TELE2"'+','+
045100030409     C                                       '"RAGIONE SOCIALE"'+','+
045200030409     C                                       '"INDIRIZZO"'+','+
045300030409     C                                       '"LOCALITA"'+','+
045400030409     C                                       '"PROVINCIA"'+','+
045500030409     C                                       '"CAP"'+','+
045600030409     C                                       '"DATA RESO"'+','+
045700030409     C                                       '"IMPORTO CONTRASSEGNO"'+','+
045800030507     C                                       '"DESCRIZIONE CAUSALE RESO"'+','+
045900030715     C                                       '"TELEFONO"'+','+
046000030715     C                                       '"DATA SPEDIZIONE"'
046100030409     C                   WRITE     TELE2X03
046200030409     C                   EVAL      flgIntXLS3 = '1'
046300030409     C                   ENDIF
046400030409     C*
046500030409     C* "Chaino" la bolla x reperire i dati mancanti
046600030409     C     KEYtas30cP    CHAIN     TITAS30C
046700030409     C                   IF        %found(TITAS30C)
046800030409     C*
046900030409     C* Reperisco la descrizioine dell'evento che caratterizza la spedizione
047000030409     C                   CLEAR                   ds2A
047100030409     C                   EVAL      tblKUT = 1
047200030409     C                   EVAL      tblCOD = '2A'
047300030430     C                   IF        depCEV <> *blanks
047400030409     C                   EVAL      tblKEY = depCEV
047500030430     C                   ELSE
047600030430     C                   EVAL      tblKEY = depCEV_2
047700030430     C                   ENDIF
047800031007     C* Siccome nn si vuole restituire al cliente l'evento 'MIC' in caso c sia lo
047900031007     C* forzo a 'G' di giacenza generica
048000031007     C                   IF        tblKEY = 'MIC'
048100031007     C                   EVAL      tblKEY = 'G'
048200031007     C                   ENDIF
048300031007     C*
048400030409     C     KEYtab        CHAIN     tabel00f
048500030409     C                   IF        %found(TABEL00F)
048600030409     C                   EVAL      ds2A = tblUNI
048700030430     C                   IF        §2AFTC = 'A'
048800030430     C                   CLEAR                   ds2A
048900030430     C                   EVAL      tblKUT = 1
049000030430     C                   EVAL      tblCOD = '2A'
049100030430     C                   EVAL      tblKEY = '017'
049200030430     C     KEYtab        CHAIN     tabel00f
049300030430     C                   IF        %found(TABEL00F)
049400030430     C                   EVAL      ds2A = tblUNI
049500030430     C                   ENDIF
049600030430     C                   ENDIF
049700030409     C                   ENDIF
049800031007     C*
049900031007     C* Controllo ridondante x eventi 'MIC'
050000031007     C                   IF        %trim(§2ADES) = 'MESSA IN CONSEGNA'
050100031007     C                   EVAL      §2ADES = 'GIACENZA GENERICA TEMPORANEA'
050200031007     C                   ENDIF
050300030409     C*
050400030409     C* Scrivo i record di dettaglio esiti
050500030409     C                   EVAL      T23XDTA = %trim(%editc(VACRMN:'Q'))+','+
050600030409     C                                          '"'+%trim(VACRMA)+'"'+','+
050700030409     C                                          '"'+%trim(VACRSD)+'"'+','+
050800030409     C                                          '"'+%trim(TASIND)+'"'+','+
050900030409     C                                          '"'+%trim(TASLOD)+'"'+','+
051000030409     C                                          '"'+%trim(TASPRD)+'"'+','+
051100030409     C                                          '"'+%trim(TASCAD)+'"'+','+
051200030409     C                                       %trim(%editc(VACDCM:'Q'))+','+
051300030430     C                                       %trim(%editc(VACCAS:'P'))+','+
051400030507     C                                          '"'+%trim(§2ADES)+'"'+','+
051500030715     C                                          '"'+%trim(VACRMO)+'"'+','+
051600030715     C                                       %trim(VACDSP)
051700030409     C*
051800030812     C                   MOVEL     'S'           FlgTELE2
051900030409     C                   WRITE     TELE2X03
052000030409     C                   WRITE     FNVACsss
052100030521     C*
052200030521     C     wVACRRN       chain     fnvac00t
052300030521     C                   if        %found(fnvac00t)
052400030521     C                   delete    FNVACTTT
052500030521     C                   endif
052600030409     C*
052700030409     C                   ENDIF
052800030409     C*
052900030409     C                   ENDSR
053000030409
053100030409
053200030409
053300030409     C     TRADXLS4      BEGSR
053400030409     C*
053500030409     C* Qualora nn ancora fatto imposto la prima riga con le intestazioni dei campi
053600030911     C                   IF        flgIntXLS4 = '0' AND wInt4 = 'S'
053700030409     C                   EVAL      T24XDTA = '"NUMERO ORDINE"'+','+
053800030410     C                                       '"CODICE CLIENTE TELE2"'+','+
053900030409     C                                       '"RAGIONE SOCIALE"'+','+
054000030409     C                                       '"INDIRIZZO"'+','+
054100030409     C                                       '"LOCALITA"'+','+
054200030409     C                                       '"PROVINCIA"'+','+
054300030409     C                                       '"CAP"'+','+
054400030411     C                                       '"DATA CONSEGNA"'+','+
054500030715     C                                       '"IMPORTO CONTRASSEGNO"'+','+
054600030715     C                                       '"DATA SPEDIZIONE"'
054700030409     C                   WRITE     TELE2X04
054800030409     C                   EVAL      flgIntXLS4 = '1'
054900030409     C                   ENDIF
055000030409     C*
055100030409     C* "Chaino" la bolla x reperire i dati mancanti
055200030409     C     KEYtas30cP    CHAIN     TITAS30C
055300030409     C                   IF        %found(TITAS30C)
055400030409     C*
055500030409     C* Scrivo i record di dettaglio esiti
055600030409     C                   EVAL      T24XDTA = %trim(%editc(VACRMN:'Q'))+','+
055700030409     C                                          '"'+%trim(VACRMA)+'"'+','+
055800030409     C                                          '"'+%trim(VACRSD)+'"'+','+
055900030409     C                                          '"'+%trim(TASIND)+'"'+','+
056000030409     C                                          '"'+%trim(TASLOD)+'"'+','+
056100030409     C                                          '"'+%trim(TASPRD)+'"'+','+
056200030409     C                                          '"'+%trim(TASCAD)+'"'+','+
056300030409     C                                       %trim(%editc(VACDCM:'Q'))+','+
056400030715     C                                       %trim(%editc(VACCAS:'P'))+','+
056500030715     C                                       %trim(VACDSP)
056600030409     C*
056700030812     C                   MOVEL     'S'           FlgTELE2
056800030812     C                   WRITE     TELE2X04
056900030409     C*
057000030409     C                   ENDIF
057100030409     C*
057200030409     C                   ENDSR
057300030812
057400030526
057500030812
057600030812     C     TRADVAC       BEGSR
057700030812     C*
057800030812     C                   MOVE      VACAAS        VHCAAS
057900030812     C                   MOVE      VACLNP        VHCLNP
058000030812     C                   MOVE      VACNRS        VHCNRS
058100030812     C                   MOVE      VACNSP        VHCNSP
058200030812     C                   MOVE      VACMGS        VHCMGS
058300030812     C                   MOVE      VACCBO        VHCCBO
058400030812     C                   MOVE      VACLNA        VHCLNA
058500030812     C                   MOVE      VACRSD        VHCRSD
058600030812     C*
058700030812     C                   MOVE      VACPRD        VHCPRD
058800030812     C                   MOVE      VACGC1        VHCGC1
058900030812     C                   MOVE      VACGC2        VHCGC2
059000030812     C                   MOVE      VACCTR        VHCCTR
059100030812     C                   MOVE      VACCTS        VHCCTS
059200030812     C                   MOVE      VACFTM        VHCFTM
059300030812     C                   MOVE      VACFIN        VHCFIN
059400030812     C                   MOVE      VACFAP        VHCFAP
059500030812     C                   MOVE      VACTSP        VHCTSP
059600030812     C*
059700030812     C                   MOVE      VACIAS        WD15
059800030812     C                   MOVEL     VACIAS        WI15
059900030812     C                   MOVE      WASC15        VHCIAS
060000030812     C*
060100030812     C                   MOVE      VACVAS        VHCVAS
060200030812     C                   MOVE      VACNAS        VHCNAS
060300030812     C                   MOVE      VACNCL        VHCNCL
060400030812     C*
060500030812     C                   MOVE      VACPKB        WD09
060600030812     C                   MOVEL     VACPKB        WI09
060700030812     C                   MOVE      WASC09        VHCPKB
060800030812     C*
060900030812     C                   MOVE      VACVLB        WD07
061000030812     C                   MOVEL     VACVLB        WI07
061100030812     C                   MOVE      WASC07        VHCVLB
061200030812     C*
061300030812     C                   MOVE      VACQFT        WD15
061400030812     C                   MOVEL     VACQFT        WI15
061500030812     C                   MOVE      WASC15        VHCQFT
061600030812     C*
061700030812     C                   MOVE      VACTIC        VHCTIC
061800030812     C*
061900030812     C                   MOVE      VACCAS        WD15
062000030812     C                   MOVEL     VACCAS        WI15
062100030812     C                   MOVE      WASC15        VHCCAS
062200030812     C*
062300030812     C                   MOVE      VACVCA        VHCVCA
062400030812     C                   MOVE      VACCCM        VHCCCM
062500030812     C                   MOVE      VACRMN        VHCRMN
062600030812     C                   MOVE      VACRMA        VHCRMA
062700030812     C                   MOVE      VACRMO        VHCRMO
062800030812     C                   MOVE      VACFFD        VHCFFD
062900030812     C                   MOVE      VACTCR        VHCTCR
063000030812     C                   MOVE      VACDCR        VHCDCR
063100030812     C                   MOVE      VACHCR        VHCHCR
063200030812     C                   MOVE      VACDCM        VHCDCM
063300030812     C                   MOVE      VACHMC        VHCHMC
063400030812     C                   MOVE      VACTC1        VHCTC1
063500030812     C                   MOVE      VACTC2        VHCTC2
063600030812     C                   MOVE      VACCCA        VHCCCA
063700030812     C                   MOVE      VACDLA        VHCDLA
063800030812     C                   MOVE      VACDAG        VHCDAG
063900030812     C*
064000030812     C                   WRITE     FNVACAA
064100030812     C*
064200030812     C                   ENDSR
064300030812
064400030812
064500030526
064600030526      /TITLE Valorizzazione Progressivo Applicazione
064700030812     C     calprog       begsr
064800030526     C*
064900030526     C                   open      tis7prgf
065000030526     C*
065100030526     C                   read(e)   tis7prgf
065200030526     C                   if        not %error
065300030526     C                   eval      dwlprg = f_tis7prgf
065400030526     C*
065500030526     C                   move(p)   dwlprg        wrkprg
065600030526     C                   add       1             wrkprg
065700030526     C                   move(p)   wrkprg        dwlprg
065800030812     C                   movel     wISV          dwlprg
065900030526     C*
066000030526     C                   eval      f_tis7prgf = dwlprg
066100030526     C                   update    tis7prg0
066200030526     C                   endif
066300030526     C*
066400030526     C                   close     tis7prgf
066500030526     C*
066600030526     C                   endsr
066700030526
066800030410
066900030410
067000030410
067100030812     C     CREALOGS      BEGSR
067200030519     C*
067300030526     C* Valorizzo i campi comuni a tutti i reord di log da "clonare"
067400030526     C                   CLEAR                   TIVLT000
067500030812     C                   EVAL      wISV = 'OE'
067600030526     C                   EVAL      vltKSC = '0' + prmksc
067700030526     C                   EVAL      vltKSU = '0' + prmksc
067800030526     C                   EVAL      vltMBD = 'M0'+ prmksc
067900030526     C                   EVAL      vltSND = 'D'
068000030526     C                   EVAL      vltFLS = 'FNVAC00S'
068100030526     C*
068200030526     C                   time                    wrkoggiiso
068300030526     C                   extrct    wrkoggiiso:*m wrkmese
068400030526     C                   extrct    wrkoggiiso:*d wrkday
068500030526     C                   eval      vltMBS =
068600030526     C                             mc(wrkmese)
068700030526     C                             + wrkdaychr
068800030526     C                             + %subst(vltKSU:2:7)
068900030526     C*
069000030526     C                   EVAL      vltDAT = DATCOR
069100030526     C                   EVAL      vltORA = ORACOR
069200030812     C                   EVAL      vltISV = wISV
069300030526     C                   EVAL      vltTIA = 'F'
069400030526     C                   EVAL      vltFL2 = '01'
069500030526     C                   EVAL      vltSTS = '0'
069600030410     C*
069700030410     C* => scrivo record di log x tipo file 2T (TELE2XLS2)
069800030526     C*
069900030526     C                   exsr      calprog
070000030526     C                   EVAL      vltPRG = dwlprg
070100030526     C*
070200030410     C                   EVAL      vltTIP = '2T'
070300030410     C                   EVAL      vltFLD = 'TELE2XLS2'
070400030410     C                   WRITE     TIVLT000
070500030410     C* => scrivo record di log x tipo file 3T (TELE2XLS3)
070600030526     C*
070700030526     C                   exsr      calprog
070800030526     C                   EVAL      vltPRG = dwlprg
070900030526     C*
071000030410     C                   EVAL      vltTIP = '3T'
071100030410     C                   EVAL      vltFLD = 'TELE2XLS3'
071200030410     C                   WRITE     TIVLT000
071300030410     C* => scrivo record di log x tipo file 4T (TELE2XLS4)
071400030526     C*
071500030526     C                   exsr      calprog
071600030526     C                   EVAL      vltPRG = dwlprg
071700030526     C*
071800030410     C                   EVAL      vltTIP = '4T'
071900030410     C                   EVAL      vltFLD = 'TELE2XLS4'
072000030410     C                   WRITE     TIVLT000
072100030812     C*
072200030812     C* => Se trattasi del cliente 0052552 scrivo record di log x tipo file VC (SIRTI)
072300030812     C*
072400030812     C                   IF        prmksc = '0052552'
072500030812     C                   CLEAR                   TIVLT000
072600030812     C                   EVAL      wISV = 'OB'
072700030813     C                   EVAL      vltKSC = '0' + prmksc2
072800030813     C                   EVAL      vltKSU = '0' + prmksc2
072900030813     C                   EVAL      vltMBD = 'M0'+ prmksc2
073000030812     C                   EVAL      vltSND = 'D'
073100030812     C                   EVAL      vltFLS = 'FNVAC00S'
073200030812     C*
073300030812     C                   time                    wrkoggiiso
073400030812     C                   extrct    wrkoggiiso:*m wrkmese
073500030812     C                   extrct    wrkoggiiso:*d wrkday
073600030812     C                   eval      vltMBS =
073700030812     C                             mc(wrkmese)
073800030812     C                             + wrkdaychr
073900030812     C                             + %subst(vltKSU:2:7)
074000030812     C*
074100030812     C                   EVAL      vltDAT = DATCOR
074200030812     C                   EVAL      vltORA = ORACOR
074300030812     C                   EVAL      vltISV = wISV
074400030812     C                   EVAL      vltTIA = 'F'
074500030812     C                   EVAL      vltFL2 = '01'
074600030812     C                   EVAL      vltSTS = '0'
074700030812     C                   exsr      calprog
074800030812     C                   EVAL      vltPRG = dwlprg
074900030812     C*
075000030812     C                   EVAL      vltTIP = 'VC'
075100030812     C                   EVAL      vltFLD = 'FNVACWWT'
075200030812     C                   WRITE     TIVLT000
075300030812     C                   ENDIF
075400030410     C*
075500030410     C                   ENDSR
075600030410
075700030410
075800030410
075900030410     C     CARTAB        BEGSR
076000030410     C*
076100030410     C                   clear                   sk2Agiac
076200030410     C                   z-add     *zeros        i                 3 0
076300030410     C                   eval      tblKUT = 1
076400030410     C                   eval      tblCOD = '2A'
076500030410     C     KEYtabP       setll     tabel00f
076600030410     C     KEYtabP       reade     tabel00f
076700030410     C                   dow       not %eof(tabel00f)
076800030410     C                   movel(p)  tblUNI        ds2a
076900030410     C                   if        §2AFTC = 'G'
077000030410     C                   add       1             i
077100030410     C                   eval      sk2Agiac(i) = tblKEY
077200030410     C                   endif
077300030410     C     KEYtabP       reade     tabel00f
077400030410     C                   enddo
077500030410     C*
077600030410     C                   ENDSR
077700030417
077800030417
077900030417
078000030417     C*----------------------------------------------------*
078100030417     C*  CONTROLLO NUMERICITA' CAMPI
078200030417     C*----------------------------------------------------*
078300030417     C     CHKNUM        BEGSR
078400030417     C*
078500030417     C                   call(e)   'ISNUMERIC'
078600030417     C                   PARM                    PiStr            30
078700030417     C                   PARM      ','           PiDecChr          1
078800030417     C                   PARM      *ZEROS        PiVal            30 9
078900030417     C                   PARM      '0'           PiInt             1
079000030417     C                   PARM      '0'           PiNum             1
079100030417     C                   IF        %error
079200030417     C                   EVAL      PiInt=*off
079300030417     C                   EVAL      PiNum=*off
079400030417     C                   ENDIF
079500030417     C*
079600030417     C                   ENDSR
079700030417     C***
079800030409
079900030410
080000030409
080100991027
080200991027      /TITLE Operazioni iniziali.
080300991027     C     *inzsr        BEGSR
080400991027     C*
080500991027     C     *ENTRY        PLIST
080600030409     C                   PARM                    prmppt
080700030409     C     wrkesito      PARM      wrkesito      prmesito
080800030521     C                   PARM                    prmksc
080900030813     C                   PARM                    prmksc2
081000030409     C*
081100030409     C* IMPOSTA LA DATA CORRENTE
081200030409     C                   TIME                    N14              14 0
081300030409     C                   MOVE      N14           N8                8 0
081400030526     C                   MOVEL     N14           ORACOR            6 0
081500030409     C                   Z-ADD     N8            G08DAT
081600030409     C                   Z-ADD     *ZEROS        G08INV
081700030409     C                   MOVEL     '0'           G08ERR
081800030409     C                   CALL      'XSRDA8'
081900030409     C                   PARM                    WLBDA8
082000030409     C                   Z-ADD     G08INV        DATCOR            8 0
082100030410     C*
082200030410     C* CARICO LE TABELLE OCCORRENTI
082300030410     C                   EXSR      CARTAB
082400030409     C****
082500030409     C* Definizione chiavi
082600030409     C****
082700030410     C* Chiave su TABEL00F - parziale
082800030410     C     KEYtabP       KLIST
082900030410     C                   KFLD                    tblKUT
083000030410     C                   KFLD                    tblCOD
083100030409     C* Chiave su TABEL00F - completa
083200030409     C     KEYtab        KLIST
083300030409     C                   KFLD                    tblKUT
083400030409     C                   KFLD                    tblCOD
083500030409     C                   KFLD                    tblKEY
083600030409     C* Chiave su FNEVB05L - parziale
083700030409     C     KEYevb05P     KLIST
083800030409     C                   KFLD                    vacAAS
083900030409     C                   KFLD                    vacLNP
084000030409     C                   KFLD                    vacNRS
084100030409     C                   KFLD                    vacNSP
084200030409     C* Chiave su TITAS30C - parziale
084300030409     C     KEYtas30cP    KLIST
084400030409     C                   KFLD                    vacAAS
084500030409     C                   KFLD                    vacLNP
084600030409     C                   KFLD                    vacNRS
084700030409     C                   KFLD                    vacNSP
084800030603     C* Chiave su FNLBL01L - completa
084900030603     C     KEYlbl01C     KLIST
085000030603     C                   KFLD                    vacAAS
085100030603     C                   KFLD                    vacLNP
085200030603     C                   KFLD                    vacNRS
085300030603     C                   KFLD                    vacNSP
085400030409     C* Chiave su TIVIR01L - completa
085500030409     C     KEYvir01      KLIST
085600030409     C                   KFLD                    virKSC
085700030409     C                   KFLD                    virSEQ
085800991027     C*
085900991027     C                   ENDSR
086000030409     C*------------------------------------------------------------
086100030409** sk2Ainderr - SCHIERA CODICI EVENTO X INDIRIZZO ERRATO
086200030409019
086300030409020
086400030409021
086500030409022
086600030409
086700030409** sk2Arif    - SCHIERA CODICI EVENTO X RIFIUTO DESTINATARIO
086800030409001
086900030409002
087000030409003
087100030409004
087200030409005
087300030409007
087400030409008
087500030409010
087600030409012
087700030409013
087800030409014
087900030409027
088000030409029
088100030409030
088200030409031
088300030409RFM
088400030409RFD
088500030409
088600030409
088700030409
088800030526** ctdata mc
088900030526ABCDEFGHILMN
