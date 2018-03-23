000100130612      /TITLE Upload via Internet: traduzione "UPL/DWL"
000200100318     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300130612     H DECEDIT('0,') DATEDIT(*DMY.)
000400000313     F*
000500990910     Ftivin00r  uF   E             DISK    usropn
000600130612     Ftitas32c  iF   E           K DISK
000700130702     Ftitas31c  iF   E           K DISK    RENAME(titas000:titas310)
000800130702     F                                     RENAME(titas010:titas311)
000900130702     F                                     RENAME(titasP00:titas31P)
001000130612     Ftita430c  iF   E           K DISK
001100130612     Ftivgd00f  O    E             DISK
001200040113     D*------------
001300040113     D* SCHIEDE A PROGRAMMA
001400040113     D*------------
001500080429     D intcol          S            100    DIM(5) CTDATA PERRCD(1)
001600010330     D*----------------------------------------------------
001700010330     D* DICHIARAZIOINE VARIABILI DI WRK
001800010330     D*----------------------------------------------------
001900010330     D psds           sds
002000010330     D  procname         *PROC
002100010330     D tivlrds       e ds                  extname(tivlr00f)
002200130612     D titasds_w     e ds                  extname(titas00f) prefix(w_) inz
002300130612     D titasds_s     e ds                  extname(titas00f) prefix(s_) inz
002400130612     D titasds_p     e ds                  extname(titas00f) prefix(p_) inz
002500130612     D trul47ds      E DS
002600010330     D esito           s              1
002700130612     D prmlit          s             10
002800130612     D prmfir          s             10
002900010330     D wrkesito        s                   like(esito)
003000130813     D wStatus         s             30    inz
003100130612     D wPorto          s             13S 2 inz
003200130612     D wPortoTot       s             13S 2 inz
003300130612     D wDati           s           2048    varying
003400130612     D w_tasRMA        s             15    inz
003500130612     D s_tasRMA        s             15    inz
003600130702     D keyKSC          s                   like(tasKSC) inz(0504102)
003700130702     D keyDFT          s                   like(tasDFT) inz
003800130702     D skRMN           s             15  0 inz dim(64000)
003900130702     D idxRMN          s              5  0 inz
004000130612     D*
004100130612     D TxtInOut        S           2048
004200130612     D ElencoChar      S            256
004300130612     D TipoElenco      S              1
004400130612     D CharSost        S              1
004500130612     D UpperCase       S              1
004600130612     D ChkNull         S              1
004700130612     D CharNull        S              1
004800130612     D EsitoBon        S              1
004900100326
005000130612
005100030715     D*------------------
005200030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
005300030715     D*------------------
005400030715     D SkSplitFLD      S             10    DIM(100)
005500030715     D SkSplitCSV      S            256    DIM(100)
005600030715     D CharCSV         S              1
005700030715     D CharTXT         S              1
005800030715     D CharNUM         S              1
005900100318     D CharSOS         S              1
006000030715     D posDa           S              3  0 INZ(*zeros)
006100030715     D posA            S              3  0 INZ(*zeros)
006200030715     D i               s              3  0 INZ(1)
006300030715     D wGiro           s              1  0 INZ(*zeros)
006400030822     D*------------------
006500030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
006600030822     D*------------------
006700030822     D posDaDft        S              3  0 INZ(*zeros)
006800030822     D posADft         S              3  0 INZ(*zeros)
006900030822     D j               s              3  0 INZ(1)
007000030822     D wGiroDft        s              1  0 INZ(*zeros)
007100100318
007200130612
007300100318     D*------------------
007400100318     D* LINKING A DEFINIZIONI ESTERNE
007500100318     D*------------------
007600100318     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
007700100318     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
007800130612
007900130612
008000130612     D*-------------------
008100130612     D* COSTANTI
008200130612     D*-------------------
008300130612     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
008400130612     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
008500130612
008600130612
008700130612     D*------------------
008800130612     D* RIDEFINIZIONE DS INPUT
008900130612     D*------------------
009000130612     D inKSU           s              8    inz('00504102')
009100130612     D inTIP           s              2    inz('CF')
009200130612     D inISV           s              2    inz('OE')
009300130612
009400010330
009500010330
009600990921     C                   reset                   esito
009700990921     C                   reset                   wrkesito
009800130612     C*
009900130612     C* Avvio blocco elaborazione TIVGD
010000130612     C                   EXSR      strTRUL47
010100130612     C*
010200130612     C* Se ok a procedere => elaboro
010300130612     C                   if        wProcedi = 'S'
010400130612     C*
010500130612     C* Effettuo elaborazione
010600130612     C                   exsr      opeini
010700130612     C                   exsr      rwvab
010800130702     C                   exsr      rdtas
010900130612     C*
011000130612     C                   endif
011100130612     C*
011200130612     C* Termino il blocco elaborazione TIVGD
011300130612     C                   EXSR      endTRUL47
011400010601     C*
011500010601     C                   seton                                        lr
011600010601
011700010601
011800010601
011900010601     C*--------------------------------------------------------
012000010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
012100010601     C*--------------------------------------------------------
012200010601     C     PREELA        BEGSR
012300010601     C*
012400010601     C* SE OCCORRE SPEDIRE IN FILIALE
012500130612     C                   if        flgGiro = '0'
012600010601     C*
012700010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
012800010601     C                   eval      flgGiro = '1'
012900010601     C*
013000010601     C                   endif
013100010601     C*
013200010601     C                   ENDSR
013300010601     C***
013400010601
013500010601
013600010601
013700010601     C*--------------------------------------------------------
013800010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
013900010601     C*--------------------------------------------------------
014000010601     C     ENDELA        BEGSR
014100040119     C*
014200000616     C*
014300010601     C                   ENDSR
014400010601     C***
014500000613
014600010601
014700010601
014800010330     C*--------------------------------------------------------
014900021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
015000010330     C*--------------------------------------------------------
015100010605     C     RWVAB         BEGSR
015200010330     C*
015300010330     C                   if        not %open(tivin00r)
015400010330     C                   open      tivin00r
015500010330     C                   endif
015600070823     C*
015700070823     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
015800070823     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
015900130612     C***                exsr      inzvar
016000130612 xxx C***                exsr      impfile
016100010330     C*
016200030822     C                   do        *HIVAL
016300010330     C*
016400030822     C                   read      tivin00r                               70
016500010618     C*
016600010618     C* Dopo ogni lettura verifico se ci sono stati record OK
016700010618     C                   if        vinflg = '1'
016800010618     C                   eval      flgOk = '1'
016900010618     C                   endif
017000040510     C*
017100040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
017200040510     C                   z-add     *zeros        wLenVINDTA        4 0
017300040510     C                   z-add     *zeros        wFlgVINDTA        1 0
017400040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
017500040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
017600040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
017700040510     C                   z-add     1             wFlgVINDTA
017800040510     C                   leave
017900040510     C                   endif
018000040510     C                   enddo
018100010618     C*
018200040510     C                   if        vindta > *blanks AND
018300040510     C                             wFlgVINDTA = 1
018400010330     C*
018500010601     C                   if        *in70 = *off and
018600010330     C                             (vinflg = *blanks
018700010330     C                              or vinflg = '0'
018800010330     C                              or vinflg = '2')
018900010330     C*
019000010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
019100010711     C                   if        vinflg = *blanks or vinflg = '0'
019200010711     C                   clear                   vinmsg
019300010711     C                   endif
019400010601     C*
019500070925     C* Aggiungo un byte a blanks all'inizio d ogni record
019600070925     C                   eval      vindta  = ' ' + %trim(vindta)
019700070925     C*
019800080527     C                   exsr      inzvar
019900100318     C                   exsr      normCSV
020000130612     C                   exsr      impfile
020100010601     C*
020200010601     C                   exsr      PREELA
020300130612     C*
020400130612     C* Scarico il buffer di output
020500030822     C                   if        wGiro = 2
020600130612     C*
020700130612     C* Eseguo il contronto solamente a rotttura di RMN cliente
020800130612     C                   if        s_tasRMN = *zeros
020900130827     C                   exsr      savTAS
021000130612     C                   else
021100130612     C                   if        s_tasRMN <> w_tasRMN
021200130612     C                   exsr      chkFAT
021300130612     C                   exsr      valVGD
021400130612     C                   exsr      wriVGD
021500130827     C                   exsr      savTAS
021600130612     C                   z-add     *zeros        wPortoTot
021700130612     C                   endif
021800130612     C                   endif
021900130612     C*
022000130612     C* Vado in sommatoria del porto cliente
022100130612     C                   eval      wPortoTot = wPortoTot + w_tasPOR
022200130612     C*
022300080513     C                   endif
022400010604     C*
022500130612     C                   if        *in31 = *off
022600010604     C                   eval      vinflg = '1'
022700010604     C                   else
022800010604     C                   eval      vinflg = '2'
022900010604     C                   endif
023000010601     C*
023100010604     C                   endif
023200010604     C*
023300010330     C                   else
023400010330     C                   eval      vinflg = '1'
023500010330     C                   endif
023600010601     C*
023700010601     C  N70              update    tivin000
023800010330     C*
023900030822     C  N70              enddo
024000010601     C*
024100130612     C* Scarico l'ultima rottura rimasta in canna
024200130612     C                   exsr      chkFAT
024300130612     C                   exsr      valVGD
024400130612     C                   exsr      wriVGD
024500130612     C*
024600130612     C* Imposto l'esito traduzione fisso a OK (errori già evidenti sul output)
024700010330     C                   eval      wrkesito = '0'
024800010330     C*
024900010330     C                   if        %open(tivin00r)
025000010330     C                   close     tivin00r
025100010330     C                   endif
025200010601     C*
025300010618     C                   if        flgGiro = '1'
025400010601     C                   exsr      endela
025500010618     C                   endif
025600010330     C*
025700010330     C                   ENDSR
025800010330     C***
025900130827
026000130827
026100130827
026200130827     C*--------------------------------------------------------
026300130827     C* SAVTAS - OPERAZIONI SALVATAGGIO DATI DA TITAS         *
026400130827     C*--------------------------------------------------------
026500130827     C     SAVTAS        BEGSR
026600130827     C*
026700130827     C                   eval      s_tasAAS = w_tasAAS
026800130827     C                   eval      s_tasRMN = w_tasRMN
026900130827     C                   eval      s_tasRMA = w_tasRMA
027000130827     C                   eval      s_tasRSD = w_tasRSD
027100130827     C                   eval      s_tasLOD = w_tasLOD
027200130827     C*
027300130827     C                   ENDSR
027400130827     C***
027500130612
027600130612
027700130612
027800130612     C*----------------------------------------------------*
027900130612     C*  CONFRONTO CON BOLLA DI SEDE
028000130612     C*----------------------------------------------------*
028100130612     C     CHKFAT        BEGSR
028200130612     C*
028300130612     C* Aggancio la bolla in sede
028400130612     C                   movel     'N'           wOK               1
028500130612     C     KEYtas32C     setll     titas32c
028600130612     C                   if        %equal(titas32c)
028700130612     C     KEYtas32C     reade     titas32c
028800130612     C                   dow       not %eof(titas32c)
028900130612     C                   movel     'S'           wOK
029000130612     C*
029100130612     C* Se richiesti applico i filtri
029200130612     C*
029300130612     C                   if        s_tasAAS > *zeros
029400130612     C                   if        s_tasAAS = tasAAS
029500130612     C                   else
029600130612     C                   movel     'N'           wOK
029700130612     C                   endif
029800130612     C                   endif
029900130612     C*
030000130612     C                   if        p_tasCCM > *zeros
030100130612     C                   if        p_tasCCM = tasCCM
030200130612     C                   else
030300130612     C                   movel     'N'           wOK
030400130612     C                   endif
030500130612     C                   endif
030600130612     C*
030700130612     C                   if        p_tasLNP > *zeros
030800130612     C                   if        p_tasLNP = tasLNP
030900130612     C                   else
031000130612     C                   movel     'N'           wOK
031100130612     C                   endif
031200130612     C                   endif
031300130612     C*
031400130612     C                   if        p_tasNRS > *zeros
031500130612     C                   if        p_tasNRS = tasNRS
031600130612     C                   else
031700130612     C                   movel     'N'           wOK
031800130612     C                   endif
031900130612     C                   endif
032000130612     C*
032100130612     C                   if        p_tasCTR > *zeros
032200130612     C                   if        p_tasCTR = tasCTR
032300130612     C                   else
032400130612     C                   movel     'N'           wOK
032500130612     C                   endif
032600130612     C                   endif
032700130612     C*
032800130612     C                   if        p_tasTBL <> *blanks
032900130612     C                   if        p_tasTBL  = tasTBL
033000130612     C                   else
033100130612     C                   movel     'N'           wOK
033200130612     C                   endif
033300130612     C                   endif
033400130612     C*
033500130612     C                   if        p_tasTSP <> *blanks
033600130612     C                   if        p_tasTSP  = tasTSP
033700130612     C                   else
033800130612     C                   movel     'N'           wOK
033900130612     C                   endif
034000130612     C                   endif
034100130612     C*
034200130612     C                   if        p_tasCTM <> *blanks
034300130612     C                   if        p_tasCTM  = tasCTM
034400130612     C                   else
034500130612     C                   movel     'N'           wOK
034600130612     C                   endif
034700130612     C                   endif
034800130612     C*
034900130612     C                   if        p_tasFFD <> *blanks
035000130612     C                   if        p_tasFFD  = tasFFD
035100130612     C                   else
035200130612     C                   movel     'N'           wOK
035300130612     C                   endif
035400130612     C                   endif
035500130813     C*
035600130813     C                   setoff                                       55
035700130813     C                   eval      s_tasDFT =  tasDFT
035800130813     C                   if        w_tasDFT <> tasDFT
035900130813     C                   seton                                        55
036000130813     C                   endif
036100130612     C*
036200130612     C* Aggancio il file estensione riferimenti bolle - tipo record 'A'
036300130612     C***                clear                   ta4NOT
036400130612     C***                eval      ta4TRC = 'A'
036500130612     C***  KEYta430      chain     tita430c
036600130612     C***                if        %found(tita430c)
036700130612     C***                if        s_tasRMA <> *blanks
036800130612     C***                if        s_tasRMA  = ta4NOT
036900130612     C***                else
037000130612     C***                movel     'N'           wOK
037100130612     C***                endif
037200130612     C***                endif
037300130612     C***                endif
037400130612     C*
037500130612     C* Se già trovato record perfettamente OK => esco dal ciclo
037600130612     C                   if        wOK = 'S'
037700130612     C                   leave
037800130612     C                   endif
037900130612     C*
038000130612     C     KEYtas32C     reade     titas32c
038100130612     C                   enddo
038200130612     C                   endif
038300130612     C*
038400130612     C* Considerazioni finali sul confronto fattura (solo su PORTO)
038500130612     C                   if        *in31
038600130612     C                   eval      wStatus = 'ERR_INPUT'
038700130612     C                   else
038800130612     C                   if        wOK <> 'S'
038900130612     C                   eval      wStatus = 'MANCA BRT'
039000130612     C                   else
039100130827     C                   eval      s_tasPKB = tasPKB
039200130827     C                   eval      s_tasPKC = tasPKC
039300130731     C                   if        tasPOR > *zeros
039400130731     C                   z-add     tasPOR        wPorto
039500130731     C                   else
039600130731     C                   z-add     tasIMV        wPorto
039700130731     C                   endif
039800130612     C                   if        wPortoTot = wPorto
039900130813     C  N55              eval      wStatus = 'CONFORME'
040000130813     C   55              eval      wStatus = 'CONFORME ALTRA FATTURA'
040100130612     C                   else
040200130813     C  N55              eval      wStatus = 'DIFFERENZA'
040300130813     C   55              eval      wStatus = 'DIFFERENZA ALTRA FATTURA'
040400130612     C                   endif
040500130612     C                   endif
040600130612     C                   endif
040700130612     C*
040800130612     C                   ENDSR
040900130612     C***
041000130702
041100130702
041200130702
041300130702     C*----------------------------------------------------*
041400130731     C*  LETTURA BOLLE FATTURATE NON INCLUSE IN LISTA CLIENTE
041500130702     C*----------------------------------------------------*
041600130702     C     RDTAS         BEGSR
041700130702     C*
041800141106     C                   move(p)   inKSU         KeyKSC
041900130702     C                   eval      keyDFT = w_tasDFT
042000130702     C*
042100130702     C                   movel     'N'           wOK               1
042200130702     C     KEYtas31C     setll     titas31c
042300130702     C                   if        %equal(titas31c)
042400130702     C     KEYtas31C     reade     titas31c
042500130702     C                   dow       not %eof(titas31c)
042600130702     C                   clear                   titasds_s
042700130702     C                   clear                   s_tasRMA
042800130702     C                   clear                   wPortoTot
042900130731     C                   if        tasPOR > *zeros
043000130702     C                   eval      wPorto = tasPOR
043100130731     C                   else
043200130731     C                   eval      wPorto = tasIMV
043300130731     C                   endif
043400130702     C*
043500130702     C* Considero solamente se bolla ha un imponibile
043600130702     C                   if        tasIMV > *zeros
043700130702     C*
043800130702     C* Verifico se spedizione corrente già inclusa in elenco cleinte
043900130702     C                   if        %lookup(tasRMN:skRMN:1) = 0
044000130702     C* Se non già presente imposto status e scarico il buffer
044100130822     C                   eval      wStatus  = 'MANCA CLI'
044200130822     C                   eval      s_tasRMN = tasRMN
044300130822     C                   eval      s_tasDFT = tasDFT
044400130827     C                   eval      s_tasPKB = tasPKB
044500130827     C                   eval      s_tasPKC = tasPKC
044600130702     C                   exsr      valVGD
044700130702     C                   exsr      wriVGD
044800130702     C                   endif
044900130702     C*
045000130702     C                   endif
045100130702     C*
045200130702     C     KEYtas31C     reade     titas31c
045300130702     C                   enddo
045400130702     C                   endif
045500130702     C*
045600130702     C                   ENDSR
045700130702     C***
045800040119
045900040119
046000040119
046100040119     C*----------------------------------------------------*
046200130612     C*  COMPOSIZIONE BUFFER RECORDS OUTPUT
046300040119     C*----------------------------------------------------*
046400130612     C     VALVGD        BEGSR
046500080513     C*
046600130612     C                   eval      wDati = %editc(tasAAS:'X')+'/'+
046700130612     C                                     %editc(tasLNP:'X')+'/'+
046800130612     C                                     %editc(tasNRS:'X')+'/'+
046900130612     C                                     %editc(tasNSP:'X')+';'+
047000130731     C                                     %subst(%editc(tasMGS:'X'):3:2)+'/'+
047100130731     C                                     %subst(%editc(tasMGS:'X'):1:2)+'/'+
047200130731     C                                     %editc(tasAAS:'X')+';'+
047300130612     C                                     %editc(s_tasRMN:'X')+';'+
047400130612     C                                      %trim(s_tasRMA)+';'+
047500130612     C                                      %trim(s_tasRSD)+';'+
047600130612     C                                      %trim(s_tasLOD)+';'+
047700130612     C                                     %editc(wPortoTot:'3')+';'+
047800130612     C                                     %editc(wPorto:'3')+';'+
047900130701     C                                      %trim(wStatus)+';'+
048000130813     C                                    %editc(wPortoTot-wPorto:'P')+';'+
048100130813     C                                      %trim(%editc(w_tasDFT:'X'))+';'+
048200130827     C                                      %trim(%editc(s_tasDFT:'X'))+';'+
048300130827     C                                     %editc(s_tasPKB:'4')+';'+
048400130827     C                                     %editc(s_tasPKC:'4')+';'
048500130612     C*
048600040119     C                   ENDSR
048700130612
048800130612
048900130612
049000130612     C*----------------------------------------------------*
049100130612     C*  SCARICAMENTO BUFFER RECORDS OUTPUT
049200130612     C*----------------------------------------------------*
049300130612     C     WRIVGD        BEGSR
049400130612     C*
049500130612     C* Scarico il buffer di output
049600130612     C                   clear                   tivgd000
049700130612     C                   eval      vgdDTA = wDati
049800130612     C                   eval      vgdTIP = inTIP
049900130612     C                   eval      vgdKSU = inKSU
050000130612     C                   eval      vgdTSC = 'WW'
050100130612     C                   eval      vgdDAT = datcor
050200130612     C                   eval      vgdPGM = 'TITV2Q4R'
050300130612     C                   write     tivgd000
050400130702     C*
050500130702     C* Memorizzo il RMN corrente
050600130702     C                   add       1             idxRMN
050700130702     C                   eval      skRMN(idxRMN) = w_tasRMN
050800130612     C*
050900130612     C                   ENDSR
051000010601
051100010601
051200010601
051300010330     C*----------------------------------------------------*
051400030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
051500010330     C*----------------------------------------------------*
051600040119     C     INZVAR        BEGSR
051700030822     C*
051800030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
051900030822     C                   CLEAR                   SkSplitCSV
052000030822     C                   EVAL      i = 1
052100030822     C                   EVAL      posDa = *zeros
052200030822     C                   EVAL      posA  = *zeros
052300080527     C*
052400130612     C                   CLEAR                   titasds_w
052500130612     C                   CLEAR                   tivgd000
052600130612     C                   CLEAR                   wPorto
052700130612     C                   CLEAR                   wStatus
052800130612     C                   CLEAR                   wDati
052900030822     C*
053000030822     C* Reimposto i valori di default
053100030822     C                   EXSR      DEFCAM
053200030822     C*
053300010330     C                   ENDSR
053400010330     C*----------------------------------------------------*
053500030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
053600010330     C*----------------------------------------------------*
053700010330     C     DEFCAM        BEGSR
053800030822     C*
053900030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
054000030715     C* e delimitatore testo.
054100030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
054200030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
054300030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
054400100318     C*
054500100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
054600100318     C                   EVAL      CharSOS = CharNUM
054700030822     C*
054800030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
054900030822     C                   EVAL      posDaDft = 1
055000030822     C                   EVAL      posADft  = 0
055100030822     C                   EVAL      wGiroDft = 0
055200030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
055300030822     C                             posDaDft > 0
055400030822     C*
055500030822     C* Gestisco il 1° giro
055600030822     C                   IF        wGiroDft = 0
055700030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
055800030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
055900030822     C* Incremento il contatore dei "giri"
056000030822     C                   EVAL      wGiroDft = 1
056100030822     C                   ELSE
056200030822     C                   EVAL      posDaDft = posADft
056300030822     C                   ENDIF
056400030822     C* Eseguo lo scan x trovare la fine del campo corrente
056500030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
056600030822     C*
056700030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
056800030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
056900030822     C                   IF        posDaDft > 0 AND
057000030822     C                             posADft  > 0
057100141106     C* KSU
057200141106     C                   IF        %subst(
057300141106     C                             %subst(vlrppt:posDaDft+1:
057400141106     C                             posADft-posDaDft-1):1:3)
057500141106     C                             = 'KSU'
057600141106     C                   EVAL      inKSU   =%trim(%subst(
057700141106     C                             %subst(vlrppt:posDaDft+1:
057800141106     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057900141106     C                   ENDIF
058000130612     C* CCM
058100030822     C                   IF        %subst(
058200030822     C                             %subst(vlrppt:posDaDft+1:
058300030822     C                             posADft-posDaDft-1):1:3)
058400030822     C                             = 'CCM'
058500030822     C                   EVAL      PiStr=%trim(%subst(
058600030822     C                             %subst(vlrppt:posDaDft+1:
058700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058800030822     C                   EXSR      CHKNUM
058900030822     C                   IF        PiInt=*on
059000130612     C                   Z-ADD     PiVal         p_tasCCM
059100030822     C                   ENDIF
059200030822     C                   ENDIF
059300030822     C* LNP
059400030822     C                   IF        %subst(
059500030822     C                             %subst(vlrppt:posDaDft+1:
059600030822     C                             posADft-posDaDft-1):1:3)
059700030822     C                             = 'LNP'
059800030822     C                   EVAL      PiStr=%trim(%subst(
059900030822     C                             %subst(vlrppt:posDaDft+1:
060000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060100030822     C                   EXSR      CHKNUM
060200030822     C                   IF        PiInt=*on
060300130612     C                   Z-ADD     PiVal         p_tasLNP
060400030822     C                   ENDIF
060500030822     C                   ENDIF
060600030822     C* NRS
060700030822     C                   IF        %subst(
060800030822     C                             %subst(vlrppt:posDaDft+1:
060900030822     C                             posADft-posDaDft-1):1:3)
061000030822     C                             = 'NRS'
061100030822     C                   EVAL      PiStr=%trim(%subst(
061200030822     C                             %subst(vlrppt:posDaDft+1:
061300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061400030822     C                   EXSR      CHKNUM
061500030822     C                   IF        PiInt=*on
061600130612     C                   Z-ADD     PiVal         p_tasNRS
061700030822     C                   ENDIF
061800030822     C                   ENDIF
061900030822     C* CTR
062000030822     C                   IF        %subst(
062100030822     C                             %subst(vlrppt:posDaDft+1:
062200030822     C                             posADft-posDaDft-1):1:3)
062300030822     C                             = 'CTR'
062400030822     C                   EVAL      PiStr=%trim(%subst(
062500030822     C                             %subst(vlrppt:posDaDft+1:
062600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062700030822     C                   EXSR      CHKNUM
062800030822     C                   IF        PiInt=*on
062900130612     C                   Z-ADD     PiVal         p_tasCTR
063000030822     C                   ENDIF
063100030822     C                   ENDIF
063200130612     C* TBL
063300030822     C                   IF        %subst(
063400030822     C                             %subst(vlrppt:posDaDft+1:
063500030822     C                             posADft-posDaDft-1):1:3)
063600130612     C                             = 'TBL'
063700130612     C                   EVAL      p_tasTBL=%trim(%subst(
063800030822     C                             %subst(vlrppt:posDaDft+1:
063900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064000030822     C                   ENDIF
064100030822     C* TSP
064200030822     C                   IF        %subst(
064300030822     C                             %subst(vlrppt:posDaDft+1:
064400030822     C                             posADft-posDaDft-1):1:3)
064500030822     C                             = 'TSP'
064600130612     C                   EVAL      p_tasTSP=%trim(%subst(
064700030822     C                             %subst(vlrppt:posDaDft+1:
064800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064900030822     C                   ENDIF
065000030822     C* CTM
065100030822     C                   IF        %subst(
065200030822     C                             %subst(vlrppt:posDaDft+1:
065300030822     C                             posADft-posDaDft-1):1:3)
065400030822     C                             = 'CTM'
065500130612     C                   EVAL      p_tasCTM=%trim(%subst(
065600030822     C                             %subst(vlrppt:posDaDft+1:
065700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065800030822     C                   ENDIF
065900030822     C* FFD
066000030822     C                   IF        %subst(
066100030822     C                             %subst(vlrppt:posDaDft+1:
066200030822     C                             posADft-posDaDft-1):1:3)
066300030822     C                             = 'FFD'
066400130612     C                   EVAL      p_tasFFD=%trim(%subst(
066500030822     C                             %subst(vlrppt:posDaDft+1:
066600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066700030822     C                   ENDIF
066800130612     C* inKSU
066900130612     C                   IF        %subst(
067000130612     C                             %subst(vlrppt:posDaDft+1:
067100130612     C                             posADft-posDaDft-1):1:3)
067200130612     C                             = 'KSU'
067300130612     C                   EVAL      inKSU=%trim(%subst(
067400130612     C                             %subst(vlrppt:posDaDft+1:
067500130612     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067600130612     C                   ENDIF
067700130612     C* inTIP
067800130612     C                   IF        %subst(
067900130612     C                             %subst(vlrppt:posDaDft+1:
068000130612     C                             posADft-posDaDft-1):1:3)
068100130612     C                             = 'TIP'
068200130612     C                   EVAL      inTIP=%trim(%subst(
068300130612     C                             %subst(vlrppt:posDaDft+1:
068400130612     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068500130612     C                   ENDIF
068600130612     C* inISV
068700130612     C                   IF        %subst(
068800130612     C                             %subst(vlrppt:posDaDft+1:
068900130612     C                             posADft-posDaDft-1):1:3)
069000130612     C                             = 'ISV'
069100130612     C                   EVAL      inISV=%trim(%subst(
069200130612     C                             %subst(vlrppt:posDaDft+1:
069300130612     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069400130612     C                   ENDIF
069500030822     C* ...
069600030822     C                   ENDIF
069700030822     C                   ENDDO
069800020204     C*
069900010330     C                   ENDSR
070000010607     C*----------------------------------------------------*
070100021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
070200010607     C*----------------------------------------------------*
070300130612     C     IMPFILE       BEGSR
070400010607     C*
070500130612     C                   SETOFF                                       31
070600010607     C*
070700030715     C**********
070800030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
070900030715     C**********
071000030715     C                   IF        wGiro = *zeros
071100040113     C*
071200040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
071300040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
071400080429     C                             %trim(intcol(3))+%trim(intcol(4))+
071500080429     C                             %trim(intcol(5))<>*blanks
071600040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
071700080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
071800080429     C                                      %trim(intcol(5))
071900070823     C     ';':CharCSV   XLATE     vindta        vindta
072000040510     C                   ENDIF
072100031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
072200031201     C     minu:maiu     XLATE     vindta        vindta
072300030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
072400030715     C                   DOW       posDa <= %len(%trim(vindta))
072500030715     C*
072600030715     C* Gestisco il 1° campo
072700030715     C                   IF        i = 1
072800030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
072900030715     C                   EVAL      posDa = 1
073000030715     C* Eseguo lo scan x trovare la fine del primo campo
073100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
073200030715     C* A questo punto "estraggo" il campo corrente
073300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
073400030715     C                                                    (posA-posDa))
073500030715     C* X i campi successivi al 1°
073600030715     C                   ELSE
073700030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
073800030715     C                   EVAL      posDa = posA + 1
073900030715     C* Eseguo lo scan x trovare la fine del campo corrente
074000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
074100030715     C* Gestisco l'ultimo campo
074200030715     C                   IF        posA = *zeros
074300030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
074400030715     C* A questo punto "estraggo" il campo corrente
074500030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
074600030715     C                                                    (posA-posDa))
074700030715     C                   LEAVE
074800030715     C                   ELSE
074900030715     C* A questo punto "estraggo" il campo corrente
075000030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
075100030715     C                                                    (posA-posDa))
075200030715     C                   ENDIF
075300030715     C                   ENDIF
075400030715     C* Incremento il contatore d campo
075500030715     C                   EVAL      i = i +1
075600030715     C                   ENDDO
075700030715     C*
075800030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
075900030715     C* delimitazione testo
076000030715     C                   EVAL      i = 1
076100030715     C                   DOW       i < %elem(SkSplitFLD)
076200030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
076300030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
076400030715     C                   EVAL      i = i + 1
076500030715     C                   ENDDO
076600030715     C                   EVAL      wGiro = 1
076700030715     C                   ELSE
076800030715     C                   EVAL      wGiro = 2
076900031204     C**********
077000031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
077100031204     C* d separatore campo
077200031204     C**********
077300031204     C                   EVAL      vindta = %trim(vindta)
077400031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
077500031204     C                   EVAL      vindta = %subst(vindta:2)
077600031204     C                   ENDDO
077700031204     C*
077800031204     C                   Z-ADD     *zeros        lunghInput        4 0
077900031204     C                   EVAL      lunghInput = %len(%trim(vindta))
078000031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
078100031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
078200031204     C                   EVAL      lunghInput = %len(%trim(vindta))
078300031204     C                   ENDDO
078400030715     C**********
078500030715     C* Effettuo lo split del campo dati d input
078600030715     C**********
078700030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
078800030715     C                   DOW       posDa <= %len(%trim(vindta))
078900030715     C*
079000030715     C* Gestisco il 1° campo
079100030715     C                   IF        i = 1
079200030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
079300030715     C                   EVAL      posDa = 1
079400030715     C* Eseguo lo scan x trovare la fine del primo campo
079500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
079600030715     C* A questo punto "estraggo" il campo corrente
079700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
079800030715     C                                                    (posA-posDa))
079900030715     C* X i campi successivi al 1°
080000030715     C                   ELSE
080100030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
080200030715     C                   EVAL      posDa = posA + 1
080300031204     C* Verifico che nn vi sia il campo nn valorizzato
080400031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
080500031204     C* Se campo nn valorizzato skippo al prossimo
080600031204     C                   EVAL      SkSplitCSV(i) = *blanks
080700031204     C                   EVAL      posA  = posA + 1
080800031204     C                   ELSE
080900030715     C* Eseguo lo scan x trovare la fine del campo corrente
081000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
081100030715     C* Gestisco l'ultimo campo
081200030715     C                   IF        posA = *zeros
081300030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
081400030715     C* A questo punto "estraggo" il campo corrente
081500030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
081600030715     C                                                    (posA-posDa))
081700030715     C                   LEAVE
081800030715     C                   ELSE
081900030715     C* A questo punto "estraggo" il campo corrente
082000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
082100030715     C                                                    (posA-posDa))
082200030715     C                   ENDIF
082300031204     C                   ENDIF
082400030715     C                   ENDIF
082500030715     C* Incremento il contatore d campo
082600030715     C                   EVAL      i = i +1
082700030715     C                   ENDDO
082800030715     C*
082900030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
083000030715     C* delimitazione testo
083100030715     C                   EVAL      i = 1
083200030715     C                   DOW       i < %elem(SkSplitCSV)
083300030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
083400030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
083500030715     C                   EVAL      i = i + 1
083600030715     C                   ENDDO
083700100326     C*
083800030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
083900030715     C                   EVAL      i = 1
084000030715     C                   DOW       i < %elem(SkSplitFLD)
084100030715     C*
084200030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
084300030715     C                   IF        SkSplitFLD(i) <> *blanks AND
084400030715     C                             SkSplitCSV(i) <> *blanks
084500070823     C*
084600130612 xxx C***  ===>  Gestione campi "particolari" dentro ciclo
084700110421     C*
084800130612     C*** RMN
084900130612     C                   IF        %trim(SkSplitFLD(i)) = 'INRMN'
085000130612     C                   EVAL      PiStr=SkSplitCSV(i)
085100130612     C                   EXSR      CHKNUM
085200130612     C                   IF        PiInt=*on
085300130612     C                   Z-ADD     PiVal         w_tasRMN
085400130612     C                   ELSE
085500130612     C                   SETON                                        31
085600130612     C                   EVAL      w_tasRMN = *zeros
085700130612     C                   EVAL      vinmsg = %trimr(vinmsg)
085800130612     C                             + ' ' + 'INRMN'
085900130612     C                   ENDIF
086000130612     C                   ENDIF
086100130612     C*** AAS
086200130612     C                   IF        %trim(SkSplitFLD(i)) = 'W_INAAS'
086300130612     C                   EVAL      PiStr=SkSplitCSV(i)
086400130612     C                   EXSR      CHKNUM
086500130612     C                   IF        PiInt=*on
086600130612     C                   Z-ADD     PiVal         wDATA             8 0
086700130612     C                   MOVEL     wDATA         w_tasAAS
086800130612     C                   ELSE
086900130612     C                   SETON                                        31
087000130612     C                   EVAL      w_tasAAS = *zeros
087100130612     C                   EVAL      vinmsg = %trimr(vinmsg)
087200130612     C                             + ' ' + 'INAAS'
087300130612     C                   ENDIF
087400130612     C                   ENDIF
087500130612     C*** POR
087600130612     C                   IF        %trim(SkSplitFLD(i)) = 'W_INPOR'
087700130612     C*
087800130612     C* Effettuo la chiamata al *pgm d "bonifica" x il campo 'W_INPOR'
087900130612     C                   EVAL      TxtInOut   = SkSplitCSV(i)
088000130612     C                   EVAL      ElencoChar = '1234567890,.'
088100130612     C                   EVAL      EsitoBon   = *blanks
088200130612     C*
088300130612     C                   CALL      'XCHKCHAR'
088400130612     C                   PARM                    TxtInOut
088500130612     C                   PARM                    ElencoChar
088600130612     C                   PARM                    TipoElenco
088700130612     C                   PARM                    CharSost
088800130612     C                   PARM                    UpperCase
088900130612     C                   PARM                    ChkNull
089000130612     C                   PARM                    CharNull
089100130612     C                   PARM                    EsitoBon
089200130612     C*
089300130612     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
089400130612     C                   IF        EsitoBon = '1'
089500130612     C                   EVAL      SkSplitCSV(i) = TxtInOut
089600130612     C                   ENDIF
089700130612     C*
089800130612     C                   EVAL      PiStr=SkSplitCSV(i)
089900130612     C                   EXSR      CHKNUM
090000130612     C                   IF        PiNum=*on
090100130612     C                   Z-ADD     PiVal         w_tasPOR
090200130612     C                   ELSE
090300130612     C                   SETON                                        31
090400130612     C                   EVAL      w_tasPOR = *zeros
090500130612     C                   EVAL      vinmsg = %trimr(vinmsg)
090600130612     C                             + ' ' + 'INPOR'
090700130612     C                   ENDIF
090800130612     C                   ENDIF
090900130612     C*** RSD
091000130612     C                   IF        %trim(SkSplitFLD(i)) = 'INRSD'
091100130612     C                   EVAL      w_tasRSD = SkSplitCSV(i)
091200110421     C                   ENDIF
091300130612     C*** LOD
091400130612     C                   IF        %trim(SkSplitFLD(i)) = 'INLOD'
091500130612     C                   EVAL      w_tasLOD = SkSplitCSV(i)
091600130612     C                   ENDIF
091700130612     C*** RMA
091800130612     C                   IF        %trim(SkSplitFLD(i)) = 'INRMA'
091900130612     C                   EVAL      w_tasRMA = SkSplitCSV(i)
092000130612     C                   ENDIF
092100130702     C*** DFT
092200130702     C                   IF        %trim(SkSplitFLD(i)) = 'INDFT'
092300130702     C                   EVAL      PiStr=SkSplitCSV(i)
092400130702     C                   EXSR      CHKNUM
092500130702     C                   IF        PiInt=*on
092600130702     C                   Z-ADD     PiVal         w_tasDFT
092700130702     C                   ELSE
092800130702     C                   SETON                                        31
092900130702     C                   EVAL      w_tasDFT = *zeros
093000130702     C                   EVAL      vinmsg = %trimr(vinmsg)
093100130702     C                             + ' ' + 'INDFT'
093200130702     C                   ENDIF
093300130702     C                   ENDIF
093400070925     C*
093500071029     C***  <===  -----------------------------------------
093600040510     C*
093700030715     C                   ENDIF
093800030715     C                   EVAL      i = i + 1
093900030715     C                   ENDDO
094000070925     C*
094100071029     C***  ===>  Gestione campi "particolari" fuori ciclo
094200130612     C*
094300070925     C*
094400071029     C***  <===  -----------------------------------------
094500070925     C*
094600040510     C*
094700130612     C* Considerazioni finali
094800130612     C*
094900130612     C                   ENDIF
095000020204     C*
095100010607     C                   ENDSR
095200010607     C*----------------------------------------------------*
095300010330
095400100318
095500100318
095600100318     C*----------------------------------------------------*
095700100318     C*  CONTROLLO NUMERICITA' CAMPI
095800100318     C*----------------------------------------------------*
095900100318     C     CHKNUM        BEGSR
096000100318     C*
096100100318     C                   IF        PiDecChr = *blanks
096200100318     C                   EVAL      PiDecChr = CharNUM
096300100318     C                   ENDIF
096400100318     C*
096500100318     C                   callp     UBISNUM_Check(PiStr
096600100318     C                                          :PiDecChr
096700100318     C                                          :PiVal
096800100318     C                                          :PiNum
096900100318     C                                          :PiInt)
097000100318     C*
097100100318     C                   ENDSR
097200010601     C***
097300010601
097400010601
097500010601
097600010601      /TITLE Invio dei dati al punto operativo.
097700010601     C     opeini        BEGSR
097800010601     C*
097900010601     C* Inizializzo flag e contatori operativi
098000010601     C                   movel     '0'           flgGiro           1
098100010601     C                   movel     '0'           flgMulti          1
098200010601     C                   movel     '1'           flgStato          1
098300010615     C                   movel     '0'           flgOk             1
098400010601     C                   z-add     *zeros        cntNonEl         10 0
098500010601     C                   z-add     *zeros        invfil            3 0
098600130612     C*
098700130612     C* Scrivo la riga di intestazione colonne del file di output
098800130612     C                   eval      wDati = 'SPED BRT;'+
098900130731     C                                     'DATA SPED BRT;'+
099000130612     C                                     'RIF_N MITT;'+
099100130612     C                                     'RIF_A MITT;'+
099200130612     C                                     'DESTINATARIO;'+
099300130612     C                                     'LOCALITA;'+
099400130612     C                                     'FATT CLIENTE;'+
099500130612     C                                     'FATT BRT;'+
099600130701     C                                     'STATO;'+
099700130813     C                                     'SQUADRATURA;'+
099800130813     C                                     'DATA FATTURA CHECK;'+
099900130827     C                                     'DATA FATTURA BRT;'+
100000130827     C                                     'PESO BOLLETTATO;'+
100100130827     C                                     'PESO RILEVATO;'
100200130813     C                   exsr      wriVGD
100300010601     C*
100400010601     C                   ENDSR
100500010601     C***
100600100318
100700100318
100800100318
100900100318     C     normCSV       BEGSR
101000100318     C*
101100100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
101200100318     C* separatori campi
101300100318     C                   if        CharTXT <> *blanks
101400100318     C                   z-add     *zeros        wLenVINDTA        4 0
101500100318     C                   z-add     *zeros        wFlgVINDTA        1 0
101600100318     C                   setoff                                       80
101700100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
101800100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
101900100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
102000100318     C                   if        not *in80
102100100318     C                   seton                                        80
102200100318     C                   else
102300100318     C                   setoff                                       80
102400100318     C                   endif
102500100318     C                   endif
102600100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
102700100318     C                             and *in80 = *on
102800100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
102900100318     C                   endif
103000100318     C                   enddo
103100100318     C                   endif
103200100318     C*
103300100318     C                   ENDSR
103400100318     C***
103500130612
103600130612
103700130612
103800130612     C     strTRUL47     BEGSR
103900130612     C*
104000130612     C                   movel     'N'           wProcedi          1
104100130612     C*
104200130612     C* Avvio il blocco elaborazione TIVGD x tipo file in questione
104300130612     C                   clear                   trul47ds
104400130612     C                   eval      d47opz  = 'I'
104500130612     C                   eval      d47tip  = inTIP
104600130612     C                   eval      d47lck  = 'N'
104700130612     C                   eval      d47chkj = 'S'
104800130612     C                   eval      d47pgm  = 'TITV2Q4R'
104900130612     C                   call      'TRUL47R'
105000130612     C                   parm                    trul47ds
105100130612     C*
105200130612     C* Se elaborazione consentita => proseguo
105300130612     C                   if        d47sts <> 'A'
105400130612     C                   movel     'S'           wProcedi
105500130612     C                   endif
105600130612     C*
105700130612     C                   ENDSR
105800130612
105900130612
106000130612
106100130612     C     endTRUL47     BEGSR
106200130612     C*
106300130612     C                   if        wProcedi = 'S'
106400130612     C*
106500130612     C* Elimino il blocco elaborazione TIVGD x tipo file in questione
106600130612     C                   clear                   trul47ds
106700130612     C                   eval      d47opz  = 'F'
106800130612     C                   eval      d47tip  = inTIP
106900130612     C                   call      'TRUL47R'
107000130612     C                   parm                    trul47ds
107100130612     C*
107200130612     C                   endif
107300130612     C*
107400130612     C                   ENDSR
107500010330
107600100326
107700010330
107800000613     C     *inzsr        BEGSR
107900990910     C*
108000990910     C     *entry        plist
108100130612     C                   parm                    tivlrds
108200990921     C                   parm      wrkesito      esito
108300000724     C                   parm                    prmlit
108400000710     C                   parm                    prmfir
108500010330     C*
108600010330     C* CALCOLA LA DATA CORRENTE
108700100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
108800100318     C                   eval      datcor = %dec(%date() : *ISO)
108900130612     C*
109000130612     C* Definizione chiavi
109100130612     C*
109200130612     C* Chiave su TITAS32C - Completa
109300130612     C     KEYtas32C     KLIST
109400130612     C                   KFLD                    s_tasRMN
109500130612     C                   KFLD                    p_tasCCM
109600130702     C*
109700130702     C* Chiave su TITAS31C - Completa
109800130702     C     KEYtas31C     KLIST
109900130702     C                   KFLD                    keyKSC
110000130702     C                   KFLD                    keyDFT
110100130612     C*
110200130612     C* Chiave su TITA430C - Completa
110300130612     C     KEYta430      KLIST
110400130612     C                   KFLD                    tasAAS
110500130612     C                   KFLD                    tasLNP
110600130612     C                   KFLD                    tasNRS
110700130612     C                   KFLD                    tasNSP
110800130612     C                   KFLD                    ta4TRC
110900000613     C*
111000000613     C                   ENDSR
111100000613     C***
111200040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
111300130702INRMN;W_INAAS;INRSD;INLOD;W_INPOR;INRMA;INDFT
111400110421
111500110315
111600110315
111700100318
