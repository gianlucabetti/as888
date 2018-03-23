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
001200130612     Ftis7prgf  uf   E             DISK    RENAME(tis7prgf:tis7prg0)
001300130612     F                                     PREFIX(f_)
001400130612     F                                     USROPN
001500040113     D*------------
001600040113     D* SCHIEDE A PROGRAMMA
001700040113     D*------------
001800080429     D intcol          S            100    DIM(5) CTDATA PERRCD(1)
001900010330     D*----------------------------------------------------
002000010330     D* DICHIARAZIOINE VARIABILI DI WRK
002100010330     D*----------------------------------------------------
002200010330     D psds           sds
002300010330     D  procname         *PROC
002400010330     D tivlrds       e ds                  extname(tivlr00f)
002500130612     D titasds_w     e ds                  extname(titas00f) prefix(w_) inz
002600130612     D titasds_s     e ds                  extname(titas00f) prefix(s_) inz
002700130612     D titasds_p     e ds                  extname(titas00f) prefix(p_) inz
002800130612     D trul47ds      E DS
002900010330     D esito           s              1
003000130612     D prmlit          s             10
003100130612     D prmfir          s             10
003200010330     D wrkesito        s                   like(esito)
003300130813     D wStatus         s             30    inz
003400130612     D wPorto          s             13S 2 inz
003500130612     D wPortoTot       s             13S 2 inz
003600130612     D wDati           s           2048    varying
003700130612     D w_tasRMA        s             15    inz
003800130612     D s_tasRMA        s             15    inz
003900130702     D keyKSC          s                   like(tasKSC) inz(0504102)
004000130702     D keyDFT          s                   like(tasDFT) inz
004100130702     D skRMN           s             15  0 inz dim(64000)
004200130702     D idxRMN          s              5  0 inz
004300130612     D*
004400130612     D TxtInOut        S           2048
004500130612     D ElencoChar      S            256
004600130612     D TipoElenco      S              1
004700130612     D CharSost        S              1
004800130612     D UpperCase       S              1
004900130612     D ChkNull         S              1
005000130612     D CharNull        S              1
005100130612     D EsitoBon        S              1
005200100326
005300130612
005400030715     D*------------------
005500030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
005600030715     D*------------------
005700030715     D SkSplitFLD      S             10    DIM(100)
005800030715     D SkSplitCSV      S            256    DIM(100)
005900030715     D CharCSV         S              1
006000030715     D CharTXT         S              1
006100030715     D CharNUM         S              1
006200100318     D CharSOS         S              1
006300030715     D posDa           S              3  0 INZ(*zeros)
006400030715     D posA            S              3  0 INZ(*zeros)
006500030715     D i               s              3  0 INZ(1)
006600030715     D wGiro           s              1  0 INZ(*zeros)
006700030822     D*------------------
006800030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
006900030822     D*------------------
007000030822     D posDaDft        S              3  0 INZ(*zeros)
007100030822     D posADft         S              3  0 INZ(*zeros)
007200030822     D j               s              3  0 INZ(1)
007300030822     D wGiroDft        s              1  0 INZ(*zeros)
007400100318
007500130612
007600100318     D*------------------
007700100318     D* LINKING A DEFINIZIONI ESTERNE
007800100318     D*------------------
007900100318     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
008000100318     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
008100130612
008200130612
008300130612     D*-------------------
008400130612     D* COSTANTI
008500130612     D*-------------------
008600130612     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
008700130612     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
008800130612
008900130612
009000130612     D*------------------
009100130612     D* RIDEFINIZIONE DS INPUT
009200130612     D*------------------
009300130612     D inKSU           s              8    inz('00504102')
009400130612     D inTIP           s              2    inz('CF')
009500130612     D inISV           s              2    inz('OE')
009600130612
009700010330
009800010330
009900990921     C                   reset                   esito
010000990921     C                   reset                   wrkesito
010100130612     C*
010200130612     C* Avvio blocco elaborazione TIVGD
010300130612     C                   EXSR      strTRUL47
010400130612     C*
010500130612     C* Se ok a procedere => elaboro
010600130612     C                   if        wProcedi = 'S'
010700130612     C*
010800130612     C* Effettuo elaborazione
010900130612     C                   exsr      opeini
011000130612     C                   exsr      rwvab
011100130702     C                   exsr      rdtas
011200130612     C*
011300130612     C                   endif
011400130612     C*
011500130612     C* Termino il blocco elaborazione TIVGD
011600130612     C                   EXSR      endTRUL47
011700010601     C*
011800010601     C                   seton                                        lr
011900010601
012000010601
012100010601
012200010601     C*--------------------------------------------------------
012300010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
012400010601     C*--------------------------------------------------------
012500010601     C     PREELA        BEGSR
012600010601     C*
012700010601     C* SE OCCORRE SPEDIRE IN FILIALE
012800130612     C                   if        flgGiro = '0'
012900010601     C*
013000010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
013100010601     C                   eval      flgGiro = '1'
013200010601     C*
013300010601     C                   endif
013400010601     C*
013500010601     C                   ENDSR
013600010601     C***
013700010601
013800010601
013900010601
014000010601     C*--------------------------------------------------------
014100010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
014200010601     C*--------------------------------------------------------
014300010601     C     ENDELA        BEGSR
014400040119     C*
014500000616     C*
014600010601     C                   ENDSR
014700010601     C***
014800000613
014900010601
015000010601
015100010330     C*--------------------------------------------------------
015200021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
015300010330     C*--------------------------------------------------------
015400010605     C     RWVAB         BEGSR
015500010330     C*
015600010330     C                   if        not %open(tivin00r)
015700010330     C                   open      tivin00r
015800010330     C                   endif
015900070823     C*
016000070823     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
016100070823     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
016200130612     C***                exsr      inzvar
016300130612 xxx C***                exsr      impfile
016400010330     C*
016500030822     C                   do        *HIVAL
016600010330     C*
016700030822     C                   read      tivin00r                               70
016800010618     C*
016900010618     C* Dopo ogni lettura verifico se ci sono stati record OK
017000010618     C                   if        vinflg = '1'
017100010618     C                   eval      flgOk = '1'
017200010618     C                   endif
017300040510     C*
017400040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
017500040510     C                   z-add     *zeros        wLenVINDTA        4 0
017600040510     C                   z-add     *zeros        wFlgVINDTA        1 0
017700040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
017800040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
017900040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
018000040510     C                   z-add     1             wFlgVINDTA
018100040510     C                   leave
018200040510     C                   endif
018300040510     C                   enddo
018400010618     C*
018500040510     C                   if        vindta > *blanks AND
018600040510     C                             wFlgVINDTA = 1
018700010330     C*
018800010601     C                   if        *in70 = *off and
018900010330     C                             (vinflg = *blanks
019000010330     C                              or vinflg = '0'
019100010330     C                              or vinflg = '2')
019200010330     C*
019300010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
019400010711     C                   if        vinflg = *blanks or vinflg = '0'
019500010711     C                   clear                   vinmsg
019600010711     C                   endif
019700010601     C*
019800070925     C* Aggiungo un byte a blanks all'inizio d ogni record
019900070925     C                   eval      vindta  = ' ' + %trim(vindta)
020000070925     C*
020100080527     C                   exsr      inzvar
020200100318     C                   exsr      normCSV
020300130612     C                   exsr      impfile
020400010601     C*
020500010601     C                   exsr      PREELA
020600130612     C*
020700130612     C* Scarico il buffer di output
020800030822     C                   if        wGiro = 2
020900130612     C*
021000130612     C* Eseguo il contronto solamente a rotttura di RMN cliente
021100130612     C                   if        s_tasRMN = *zeros
021200130827     C                   exsr      savTAS
021300130612     C                   else
021400130612     C                   if        s_tasRMN <> w_tasRMN
021500130612     C                   exsr      chkFAT
021600130612     C                   exsr      valVGD
021700130612     C                   exsr      wriVGD
021800130827     C                   exsr      savTAS
021900130612     C                   z-add     *zeros        wPortoTot
022000130612     C                   endif
022100130612     C                   endif
022200130612     C*
022300130612     C* Vado in sommatoria del porto cliente
022400130612     C                   eval      wPortoTot = wPortoTot + w_tasPOR
022500130612     C*
022600080513     C                   endif
022700010604     C*
022800130612     C                   if        *in31 = *off
022900010604     C                   eval      vinflg = '1'
023000010604     C                   else
023100010604     C                   eval      vinflg = '2'
023200010604     C                   endif
023300010601     C*
023400010604     C                   endif
023500010604     C*
023600010330     C                   else
023700010330     C                   eval      vinflg = '1'
023800010330     C                   endif
023900010601     C*
024000010601     C  N70              update    tivin000
024100010330     C*
024200030822     C  N70              enddo
024300010601     C*
024400130612     C* Scarico l'ultima rottura rimasta in canna
024500130612     C                   exsr      chkFAT
024600130612     C                   exsr      valVGD
024700130612     C                   exsr      wriVGD
024800130612     C*
024900130612     C* Imposto l'esito traduzione fisso a OK (errori già evidenti sul output)
025000010330     C                   eval      wrkesito = '0'
025100010330     C*
025200010330     C                   if        %open(tivin00r)
025300010330     C                   close     tivin00r
025400010330     C                   endif
025500010601     C*
025600010618     C                   if        flgGiro = '1'
025700010601     C                   exsr      endela
025800010618     C                   endif
025900010330     C*
026000010330     C                   ENDSR
026100010330     C***
026200130827
026300130827
026400130827
026500130827     C*--------------------------------------------------------
026600130827     C* SAVTAS - OPERAZIONI SALVATAGGIO DATI DA TITAS         *
026700130827     C*--------------------------------------------------------
026800130827     C     SAVTAS        BEGSR
026900130827     C*
027000130827     C                   eval      s_tasAAS = w_tasAAS
027100130827     C                   eval      s_tasRMN = w_tasRMN
027200130827     C                   eval      s_tasRMA = w_tasRMA
027300130827     C                   eval      s_tasRSD = w_tasRSD
027400130827     C                   eval      s_tasLOD = w_tasLOD
027500130827     C*
027600130827     C                   ENDSR
027700130827     C***
027800130612
027900130612
028000130612
028100130612     C*----------------------------------------------------*
028200130612     C*  CONFRONTO CON BOLLA DI SEDE
028300130612     C*----------------------------------------------------*
028400130612     C     CHKFAT        BEGSR
028500130612     C*
028600130612     C* Aggancio la bolla in sede
028700130612     C                   movel     'N'           wOK               1
028800130612     C     KEYtas32C     setll     titas32c
028900130612     C                   if        %equal(titas32c)
029000130612     C     KEYtas32C     reade     titas32c
029100130612     C                   dow       not %eof(titas32c)
029200130612     C                   movel     'S'           wOK
029300130612     C*
029400130612     C* Se richiesti applico i filtri
029500130612     C*
029600130612     C                   if        s_tasAAS > *zeros
029700130612     C                   if        s_tasAAS = tasAAS
029800130612     C                   else
029900130612     C                   movel     'N'           wOK
030000130612     C                   endif
030100130612     C                   endif
030200130612     C*
030300130612     C                   if        p_tasCCM > *zeros
030400130612     C                   if        p_tasCCM = tasCCM
030500130612     C                   else
030600130612     C                   movel     'N'           wOK
030700130612     C                   endif
030800130612     C                   endif
030900130612     C*
031000130612     C                   if        p_tasLNP > *zeros
031100130612     C                   if        p_tasLNP = tasLNP
031200130612     C                   else
031300130612     C                   movel     'N'           wOK
031400130612     C                   endif
031500130612     C                   endif
031600130612     C*
031700130612     C                   if        p_tasNRS > *zeros
031800130612     C                   if        p_tasNRS = tasNRS
031900130612     C                   else
032000130612     C                   movel     'N'           wOK
032100130612     C                   endif
032200130612     C                   endif
032300130612     C*
032400130612     C                   if        p_tasCTR > *zeros
032500130612     C                   if        p_tasCTR = tasCTR
032600130612     C                   else
032700130612     C                   movel     'N'           wOK
032800130612     C                   endif
032900130612     C                   endif
033000130612     C*
033100130612     C                   if        p_tasTBL <> *blanks
033200130612     C                   if        p_tasTBL  = tasTBL
033300130612     C                   else
033400130612     C                   movel     'N'           wOK
033500130612     C                   endif
033600130612     C                   endif
033700130612     C*
033800130612     C                   if        p_tasTSP <> *blanks
033900130612     C                   if        p_tasTSP  = tasTSP
034000130612     C                   else
034100130612     C                   movel     'N'           wOK
034200130612     C                   endif
034300130612     C                   endif
034400130612     C*
034500130612     C                   if        p_tasCTM <> *blanks
034600130612     C                   if        p_tasCTM  = tasCTM
034700130612     C                   else
034800130612     C                   movel     'N'           wOK
034900130612     C                   endif
035000130612     C                   endif
035100130612     C*
035200130612     C                   if        p_tasFFD <> *blanks
035300130612     C                   if        p_tasFFD  = tasFFD
035400130612     C                   else
035500130612     C                   movel     'N'           wOK
035600130612     C                   endif
035700130612     C                   endif
035800130813     C*
035900130813     C                   setoff                                       55
036000130813     C                   eval      s_tasDFT =  tasDFT
036100130813     C                   if        w_tasDFT <> tasDFT
036200130813     C                   seton                                        55
036300130813     C                   endif
036400130612     C*
036500130612     C* Aggancio il file estensione riferimenti bolle - tipo record 'A'
036600130612     C***                clear                   ta4NOT
036700130612     C***                eval      ta4TRC = 'A'
036800130612     C***  KEYta430      chain     tita430c
036900130612     C***                if        %found(tita430c)
037000130612     C***                if        s_tasRMA <> *blanks
037100130612     C***                if        s_tasRMA  = ta4NOT
037200130612     C***                else
037300130612     C***                movel     'N'           wOK
037400130612     C***                endif
037500130612     C***                endif
037600130612     C***                endif
037700130612     C*
037800130612     C* Se già trovato record perfettamente OK => esco dal ciclo
037900130612     C                   if        wOK = 'S'
038000130612     C                   leave
038100130612     C                   endif
038200130612     C*
038300130612     C     KEYtas32C     reade     titas32c
038400130612     C                   enddo
038500130612     C                   endif
038600130612     C*
038700130612     C* Considerazioni finali sul confronto fattura (solo su PORTO)
038800130612     C                   if        *in31
038900130612     C                   eval      wStatus = 'ERR_INPUT'
039000130612     C                   else
039100130612     C                   if        wOK <> 'S'
039200130612     C                   eval      wStatus = 'MANCA BRT'
039300130612     C                   else
039400130827     C                   eval      s_tasPKB = tasPKB
039500130827     C                   eval      s_tasPKC = tasPKC
039600130731     C                   if        tasPOR > *zeros
039700130731     C                   z-add     tasPOR        wPorto
039800130731     C                   else
039900130731     C                   z-add     tasIMV        wPorto
040000130731     C                   endif
040100130612     C                   if        wPortoTot = wPorto
040200130813     C  N55              eval      wStatus = 'CONFORME'
040300130813     C   55              eval      wStatus = 'CONFORME ALTRA FATTURA'
040400130612     C                   else
040500130813     C  N55              eval      wStatus = 'DIFFERENZA'
040600130813     C   55              eval      wStatus = 'DIFFERENZA ALTRA FATTURA'
040700130612     C                   endif
040800130612     C                   endif
040900130612     C                   endif
041000130612     C*
041100130612     C                   ENDSR
041200130612     C***
041300130702
041400130702
041500130702
041600130702     C*----------------------------------------------------*
041700130731     C*  LETTURA BOLLE FATTURATE NON INCLUSE IN LISTA CLIENTE
041800130702     C*----------------------------------------------------*
041900130702     C     RDTAS         BEGSR
042000130702     C*
042100141106     C                   move(p)   inKSU         KeyKSC
042200130702     C                   eval      keyDFT = w_tasDFT
042300130702     C*
042400130702     C                   movel     'N'           wOK               1
042500130702     C     KEYtas31C     setll     titas31c
042600130702     C                   if        %equal(titas31c)
042700130702     C     KEYtas31C     reade     titas31c
042800130702     C                   dow       not %eof(titas31c)
042900130702     C                   clear                   titasds_s
043000130702     C                   clear                   s_tasRMA
043100130702     C                   clear                   wPortoTot
043200130731     C                   if        tasPOR > *zeros
043300130702     C                   eval      wPorto = tasPOR
043400130731     C                   else
043500130731     C                   eval      wPorto = tasIMV
043600130731     C                   endif
043700130702     C*
043800130702     C* Considero solamente se bolla ha un imponibile
043900130702     C                   if        tasIMV > *zeros
044000130702     C*
044100130702     C* Verifico se spedizione corrente già inclusa in elenco cleinte
044200130702     C                   if        %lookup(tasRMN:skRMN:1) = 0
044300130702     C* Se non già presente imposto status e scarico il buffer
044400130822     C                   eval      wStatus  = 'MANCA CLI'
044500130822     C                   eval      s_tasRMN = tasRMN
044600130822     C                   eval      s_tasDFT = tasDFT
044700130827     C                   eval      s_tasPKB = tasPKB
044800130827     C                   eval      s_tasPKC = tasPKC
044900130702     C                   exsr      valVGD
045000130702     C                   exsr      wriVGD
045100130702     C                   endif
045200130702     C*
045300130702     C                   endif
045400130702     C*
045500130702     C     KEYtas31C     reade     titas31c
045600130702     C                   enddo
045700130702     C                   endif
045800130702     C*
045900130702     C                   ENDSR
046000130702     C***
046100040119
046200040119
046300040119
046400040119     C*----------------------------------------------------*
046500130612     C*  COMPOSIZIONE BUFFER RECORDS OUTPUT
046600040119     C*----------------------------------------------------*
046700130612     C     VALVGD        BEGSR
046800080513     C*
046900130612     C                   eval      wDati = %editc(tasAAS:'X')+'/'+
047000130612     C                                     %editc(tasLNP:'X')+'/'+
047100130612     C                                     %editc(tasNRS:'X')+'/'+
047200130612     C                                     %editc(tasNSP:'X')+';'+
047300130731     C                                     %subst(%editc(tasMGS:'X'):3:2)+'/'+
047400130731     C                                     %subst(%editc(tasMGS:'X'):1:2)+'/'+
047500130731     C                                     %editc(tasAAS:'X')+';'+
047600130612     C                                     %editc(s_tasRMN:'X')+';'+
047700130612     C                                      %trim(s_tasRMA)+';'+
047800130612     C                                      %trim(s_tasRSD)+';'+
047900130612     C                                      %trim(s_tasLOD)+';'+
048000130612     C                                     %editc(wPortoTot:'3')+';'+
048100130612     C                                     %editc(wPorto:'3')+';'+
048200130701     C                                      %trim(wStatus)+';'+
048300130813     C                                    %editc(wPortoTot-wPorto:'P')+';'+
048400130813     C                                      %trim(%editc(w_tasDFT:'X'))+';'+
048500130827     C                                      %trim(%editc(s_tasDFT:'X'))+';'+
048600130827     C                                     %editc(s_tasPKB:'4')+';'+
048700130827     C                                     %editc(s_tasPKC:'4')+';'
048800130612     C*
048900040119     C                   ENDSR
049000130612
049100130612
049200130612
049300130612     C*----------------------------------------------------*
049400130612     C*  SCARICAMENTO BUFFER RECORDS OUTPUT
049500130612     C*----------------------------------------------------*
049600130612     C     WRIVGD        BEGSR
049700130612     C*
049800130612     C* Scarico il buffer di output
049900130612     C                   clear                   tivgd000
050000130612     C                   eval      vgdDTA = wDati
050100130612     C                   eval      vgdTIP = inTIP
050200130612     C                   eval      vgdKSU = inKSU
050300130612     C                   eval      vgdTSC = 'WW'
050400130612     C                   eval      vgdDAT = datcor
050500130612     C                   eval      vgdPGM = 'TITV2Q4R'
050600130612     C                   write     tivgd000
050700130702     C*
050800130702     C* Memorizzo il RMN corrente
050900130702     C                   add       1             idxRMN
051000130702     C                   eval      skRMN(idxRMN) = w_tasRMN
051100130612     C*
051200130612     C                   ENDSR
051300010601
051400010601
051500010601
051600010330     C*----------------------------------------------------*
051700030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
051800010330     C*----------------------------------------------------*
051900040119     C     INZVAR        BEGSR
052000030822     C*
052100030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
052200030822     C                   CLEAR                   SkSplitCSV
052300030822     C                   EVAL      i = 1
052400030822     C                   EVAL      posDa = *zeros
052500030822     C                   EVAL      posA  = *zeros
052600080527     C*
052700130612     C                   CLEAR                   titasds_w
052800130612     C                   CLEAR                   tivgd000
052900130612     C                   CLEAR                   wPorto
053000130612     C                   CLEAR                   wStatus
053100130612     C                   CLEAR                   wDati
053200030822     C*
053300030822     C* Reimposto i valori di default
053400030822     C                   EXSR      DEFCAM
053500030822     C*
053600010330     C                   ENDSR
053700010330     C*----------------------------------------------------*
053800030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
053900010330     C*----------------------------------------------------*
054000010330     C     DEFCAM        BEGSR
054100030822     C*
054200030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
054300030715     C* e delimitatore testo.
054400030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
054500030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
054600030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
054700100318     C*
054800100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
054900100318     C                   EVAL      CharSOS = CharNUM
055000030822     C*
055100030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
055200030822     C                   EVAL      posDaDft = 1
055300030822     C                   EVAL      posADft  = 0
055400030822     C                   EVAL      wGiroDft = 0
055500030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
055600030822     C                             posDaDft > 0
055700030822     C*
055800030822     C* Gestisco il 1° giro
055900030822     C                   IF        wGiroDft = 0
056000030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
056100030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
056200030822     C* Incremento il contatore dei "giri"
056300030822     C                   EVAL      wGiroDft = 1
056400030822     C                   ELSE
056500030822     C                   EVAL      posDaDft = posADft
056600030822     C                   ENDIF
056700030822     C* Eseguo lo scan x trovare la fine del campo corrente
056800030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
056900030822     C*
057000030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
057100030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
057200030822     C                   IF        posDaDft > 0 AND
057300030822     C                             posADft  > 0
057400141106     C* KSU
057500141106     C                   IF        %subst(
057600141106     C                             %subst(vlrppt:posDaDft+1:
057700141106     C                             posADft-posDaDft-1):1:3)
057800141106     C                             = 'KSU'
057900141106     C                   EVAL      inKSU   =%trim(%subst(
058000141106     C                             %subst(vlrppt:posDaDft+1:
058100141106     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058200141106     C                   ENDIF
058300130612     C* CCM
058400030822     C                   IF        %subst(
058500030822     C                             %subst(vlrppt:posDaDft+1:
058600030822     C                             posADft-posDaDft-1):1:3)
058700030822     C                             = 'CCM'
058800030822     C                   EVAL      PiStr=%trim(%subst(
058900030822     C                             %subst(vlrppt:posDaDft+1:
059000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059100030822     C                   EXSR      CHKNUM
059200030822     C                   IF        PiInt=*on
059300130612     C                   Z-ADD     PiVal         p_tasCCM
059400030822     C                   ENDIF
059500030822     C                   ENDIF
059600030822     C* LNP
059700030822     C                   IF        %subst(
059800030822     C                             %subst(vlrppt:posDaDft+1:
059900030822     C                             posADft-posDaDft-1):1:3)
060000030822     C                             = 'LNP'
060100030822     C                   EVAL      PiStr=%trim(%subst(
060200030822     C                             %subst(vlrppt:posDaDft+1:
060300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060400030822     C                   EXSR      CHKNUM
060500030822     C                   IF        PiInt=*on
060600130612     C                   Z-ADD     PiVal         p_tasLNP
060700030822     C                   ENDIF
060800030822     C                   ENDIF
060900030822     C* NRS
061000030822     C                   IF        %subst(
061100030822     C                             %subst(vlrppt:posDaDft+1:
061200030822     C                             posADft-posDaDft-1):1:3)
061300030822     C                             = 'NRS'
061400030822     C                   EVAL      PiStr=%trim(%subst(
061500030822     C                             %subst(vlrppt:posDaDft+1:
061600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061700030822     C                   EXSR      CHKNUM
061800030822     C                   IF        PiInt=*on
061900130612     C                   Z-ADD     PiVal         p_tasNRS
062000030822     C                   ENDIF
062100030822     C                   ENDIF
062200030822     C* CTR
062300030822     C                   IF        %subst(
062400030822     C                             %subst(vlrppt:posDaDft+1:
062500030822     C                             posADft-posDaDft-1):1:3)
062600030822     C                             = 'CTR'
062700030822     C                   EVAL      PiStr=%trim(%subst(
062800030822     C                             %subst(vlrppt:posDaDft+1:
062900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063000030822     C                   EXSR      CHKNUM
063100030822     C                   IF        PiInt=*on
063200130612     C                   Z-ADD     PiVal         p_tasCTR
063300030822     C                   ENDIF
063400030822     C                   ENDIF
063500130612     C* TBL
063600030822     C                   IF        %subst(
063700030822     C                             %subst(vlrppt:posDaDft+1:
063800030822     C                             posADft-posDaDft-1):1:3)
063900130612     C                             = 'TBL'
064000130612     C                   EVAL      p_tasTBL=%trim(%subst(
064100030822     C                             %subst(vlrppt:posDaDft+1:
064200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064300030822     C                   ENDIF
064400030822     C* TSP
064500030822     C                   IF        %subst(
064600030822     C                             %subst(vlrppt:posDaDft+1:
064700030822     C                             posADft-posDaDft-1):1:3)
064800030822     C                             = 'TSP'
064900130612     C                   EVAL      p_tasTSP=%trim(%subst(
065000030822     C                             %subst(vlrppt:posDaDft+1:
065100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065200030822     C                   ENDIF
065300030822     C* CTM
065400030822     C                   IF        %subst(
065500030822     C                             %subst(vlrppt:posDaDft+1:
065600030822     C                             posADft-posDaDft-1):1:3)
065700030822     C                             = 'CTM'
065800130612     C                   EVAL      p_tasCTM=%trim(%subst(
065900030822     C                             %subst(vlrppt:posDaDft+1:
066000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066100030822     C                   ENDIF
066200030822     C* FFD
066300030822     C                   IF        %subst(
066400030822     C                             %subst(vlrppt:posDaDft+1:
066500030822     C                             posADft-posDaDft-1):1:3)
066600030822     C                             = 'FFD'
066700130612     C                   EVAL      p_tasFFD=%trim(%subst(
066800030822     C                             %subst(vlrppt:posDaDft+1:
066900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067000030822     C                   ENDIF
067100130612     C* inKSU
067200130612     C                   IF        %subst(
067300130612     C                             %subst(vlrppt:posDaDft+1:
067400130612     C                             posADft-posDaDft-1):1:3)
067500130612     C                             = 'KSU'
067600130612     C                   EVAL      inKSU=%trim(%subst(
067700130612     C                             %subst(vlrppt:posDaDft+1:
067800130612     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067900130612     C                   ENDIF
068000130612     C* inTIP
068100130612     C                   IF        %subst(
068200130612     C                             %subst(vlrppt:posDaDft+1:
068300130612     C                             posADft-posDaDft-1):1:3)
068400130612     C                             = 'TIP'
068500130612     C                   EVAL      inTIP=%trim(%subst(
068600130612     C                             %subst(vlrppt:posDaDft+1:
068700130612     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068800130612     C                   ENDIF
068900130612     C* inISV
069000130612     C                   IF        %subst(
069100130612     C                             %subst(vlrppt:posDaDft+1:
069200130612     C                             posADft-posDaDft-1):1:3)
069300130612     C                             = 'ISV'
069400130612     C                   EVAL      inISV=%trim(%subst(
069500130612     C                             %subst(vlrppt:posDaDft+1:
069600130612     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069700130612     C                   ENDIF
069800030822     C* ...
069900030822     C                   ENDIF
070000030822     C                   ENDDO
070100020204     C*
070200010330     C                   ENDSR
070300010607     C*----------------------------------------------------*
070400021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
070500010607     C*----------------------------------------------------*
070600130612     C     IMPFILE       BEGSR
070700010607     C*
070800130612     C                   SETOFF                                       31
070900010607     C*
071000030715     C**********
071100030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
071200030715     C**********
071300030715     C                   IF        wGiro = *zeros
071400040113     C*
071500040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
071600040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
071700080429     C                             %trim(intcol(3))+%trim(intcol(4))+
071800080429     C                             %trim(intcol(5))<>*blanks
071900040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
072000080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
072100080429     C                                      %trim(intcol(5))
072200070823     C     ';':CharCSV   XLATE     vindta        vindta
072300040510     C                   ENDIF
072400031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
072500031201     C     minu:maiu     XLATE     vindta        vindta
072600030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
072700030715     C                   DOW       posDa <= %len(%trim(vindta))
072800030715     C*
072900030715     C* Gestisco il 1° campo
073000030715     C                   IF        i = 1
073100030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
073200030715     C                   EVAL      posDa = 1
073300030715     C* Eseguo lo scan x trovare la fine del primo campo
073400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
073500030715     C* A questo punto "estraggo" il campo corrente
073600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
073700030715     C                                                    (posA-posDa))
073800030715     C* X i campi successivi al 1°
073900030715     C                   ELSE
074000030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
074100030715     C                   EVAL      posDa = posA + 1
074200030715     C* Eseguo lo scan x trovare la fine del campo corrente
074300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
074400030715     C* Gestisco l'ultimo campo
074500030715     C                   IF        posA = *zeros
074600030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
074700030715     C* A questo punto "estraggo" il campo corrente
074800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
074900030715     C                                                    (posA-posDa))
075000030715     C                   LEAVE
075100030715     C                   ELSE
075200030715     C* A questo punto "estraggo" il campo corrente
075300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
075400030715     C                                                    (posA-posDa))
075500030715     C                   ENDIF
075600030715     C                   ENDIF
075700030715     C* Incremento il contatore d campo
075800030715     C                   EVAL      i = i +1
075900030715     C                   ENDDO
076000030715     C*
076100030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
076200030715     C* delimitazione testo
076300030715     C                   EVAL      i = 1
076400030715     C                   DOW       i < %elem(SkSplitFLD)
076500030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
076600030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
076700030715     C                   EVAL      i = i + 1
076800030715     C                   ENDDO
076900030715     C                   EVAL      wGiro = 1
077000030715     C                   ELSE
077100030715     C                   EVAL      wGiro = 2
077200031204     C**********
077300031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
077400031204     C* d separatore campo
077500031204     C**********
077600031204     C                   EVAL      vindta = %trim(vindta)
077700031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
077800031204     C                   EVAL      vindta = %subst(vindta:2)
077900031204     C                   ENDDO
078000031204     C*
078100031204     C                   Z-ADD     *zeros        lunghInput        4 0
078200031204     C                   EVAL      lunghInput = %len(%trim(vindta))
078300031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
078400031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
078500031204     C                   EVAL      lunghInput = %len(%trim(vindta))
078600031204     C                   ENDDO
078700030715     C**********
078800030715     C* Effettuo lo split del campo dati d input
078900030715     C**********
079000030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
079100030715     C                   DOW       posDa <= %len(%trim(vindta))
079200030715     C*
079300030715     C* Gestisco il 1° campo
079400030715     C                   IF        i = 1
079500030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
079600030715     C                   EVAL      posDa = 1
079700030715     C* Eseguo lo scan x trovare la fine del primo campo
079800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
079900030715     C* A questo punto "estraggo" il campo corrente
080000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
080100030715     C                                                    (posA-posDa))
080200030715     C* X i campi successivi al 1°
080300030715     C                   ELSE
080400030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
080500030715     C                   EVAL      posDa = posA + 1
080600031204     C* Verifico che nn vi sia il campo nn valorizzato
080700031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
080800031204     C* Se campo nn valorizzato skippo al prossimo
080900031204     C                   EVAL      SkSplitCSV(i) = *blanks
081000031204     C                   EVAL      posA  = posA + 1
081100031204     C                   ELSE
081200030715     C* Eseguo lo scan x trovare la fine del campo corrente
081300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
081400030715     C* Gestisco l'ultimo campo
081500030715     C                   IF        posA = *zeros
081600030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
081700030715     C* A questo punto "estraggo" il campo corrente
081800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
081900030715     C                                                    (posA-posDa))
082000030715     C                   LEAVE
082100030715     C                   ELSE
082200030715     C* A questo punto "estraggo" il campo corrente
082300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
082400030715     C                                                    (posA-posDa))
082500030715     C                   ENDIF
082600031204     C                   ENDIF
082700030715     C                   ENDIF
082800030715     C* Incremento il contatore d campo
082900030715     C                   EVAL      i = i +1
083000030715     C                   ENDDO
083100030715     C*
083200030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
083300030715     C* delimitazione testo
083400030715     C                   EVAL      i = 1
083500030715     C                   DOW       i < %elem(SkSplitCSV)
083600030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
083700030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
083800030715     C                   EVAL      i = i + 1
083900030715     C                   ENDDO
084000100326     C*
084100030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
084200030715     C                   EVAL      i = 1
084300030715     C                   DOW       i < %elem(SkSplitFLD)
084400030715     C*
084500030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
084600030715     C                   IF        SkSplitFLD(i) <> *blanks AND
084700030715     C                             SkSplitCSV(i) <> *blanks
084800070823     C*
084900130612 xxx C***  ===>  Gestione campi "particolari" dentro ciclo
085000110421     C*
085100130612     C*** RMN
085200130612     C                   IF        %trim(SkSplitFLD(i)) = 'INRMN'
085300130612     C                   EVAL      PiStr=SkSplitCSV(i)
085400130612     C                   EXSR      CHKNUM
085500130612     C                   IF        PiInt=*on
085600130612     C                   Z-ADD     PiVal         w_tasRMN
085700130612     C                   ELSE
085800130612     C                   SETON                                        31
085900130612     C                   EVAL      w_tasRMN = *zeros
086000130612     C                   EVAL      vinmsg = %trimr(vinmsg)
086100130612     C                             + ' ' + 'INRMN'
086200130612     C                   ENDIF
086300130612     C                   ENDIF
086400130612     C*** AAS
086500130612     C                   IF        %trim(SkSplitFLD(i)) = 'W_INAAS'
086600130612     C                   EVAL      PiStr=SkSplitCSV(i)
086700130612     C                   EXSR      CHKNUM
086800130612     C                   IF        PiInt=*on
086900130612     C                   Z-ADD     PiVal         wDATA             8 0
087000130612     C                   MOVEL     wDATA         w_tasAAS
087100130612     C                   ELSE
087200130612     C                   SETON                                        31
087300130612     C                   EVAL      w_tasAAS = *zeros
087400130612     C                   EVAL      vinmsg = %trimr(vinmsg)
087500130612     C                             + ' ' + 'INAAS'
087600130612     C                   ENDIF
087700130612     C                   ENDIF
087800130612     C*** POR
087900130612     C                   IF        %trim(SkSplitFLD(i)) = 'W_INPOR'
088000130612     C*
088100130612     C* Effettuo la chiamata al *pgm d "bonifica" x il campo 'W_INPOR'
088200130612     C                   EVAL      TxtInOut   = SkSplitCSV(i)
088300130612     C                   EVAL      ElencoChar = '1234567890,.'
088400130612     C                   EVAL      EsitoBon   = *blanks
088500130612     C*
088600130612     C                   CALL      'XCHKCHAR'
088700130612     C                   PARM                    TxtInOut
088800130612     C                   PARM                    ElencoChar
088900130612     C                   PARM                    TipoElenco
089000130612     C                   PARM                    CharSost
089100130612     C                   PARM                    UpperCase
089200130612     C                   PARM                    ChkNull
089300130612     C                   PARM                    CharNull
089400130612     C                   PARM                    EsitoBon
089500130612     C*
089600130612     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
089700130612     C                   IF        EsitoBon = '1'
089800130612     C                   EVAL      SkSplitCSV(i) = TxtInOut
089900130612     C                   ENDIF
090000130612     C*
090100130612     C                   EVAL      PiStr=SkSplitCSV(i)
090200130612     C                   EXSR      CHKNUM
090300130612     C                   IF        PiNum=*on
090400130612     C                   Z-ADD     PiVal         w_tasPOR
090500130612     C                   ELSE
090600130612     C                   SETON                                        31
090700130612     C                   EVAL      w_tasPOR = *zeros
090800130612     C                   EVAL      vinmsg = %trimr(vinmsg)
090900130612     C                             + ' ' + 'INPOR'
091000130612     C                   ENDIF
091100130612     C                   ENDIF
091200130612     C*** RSD
091300130612     C                   IF        %trim(SkSplitFLD(i)) = 'INRSD'
091400130612     C                   EVAL      w_tasRSD = SkSplitCSV(i)
091500110421     C                   ENDIF
091600130612     C*** LOD
091700130612     C                   IF        %trim(SkSplitFLD(i)) = 'INLOD'
091800130612     C                   EVAL      w_tasLOD = SkSplitCSV(i)
091900130612     C                   ENDIF
092000130612     C*** RMA
092100130612     C                   IF        %trim(SkSplitFLD(i)) = 'INRMA'
092200130612     C                   EVAL      w_tasRMA = SkSplitCSV(i)
092300130612     C                   ENDIF
092400130702     C*** DFT
092500130702     C                   IF        %trim(SkSplitFLD(i)) = 'INDFT'
092600130702     C                   EVAL      PiStr=SkSplitCSV(i)
092700130702     C                   EXSR      CHKNUM
092800130702     C                   IF        PiInt=*on
092900130702     C                   Z-ADD     PiVal         w_tasDFT
093000130702     C                   ELSE
093100130702     C                   SETON                                        31
093200130702     C                   EVAL      w_tasDFT = *zeros
093300130702     C                   EVAL      vinmsg = %trimr(vinmsg)
093400130702     C                             + ' ' + 'INDFT'
093500130702     C                   ENDIF
093600130702     C                   ENDIF
093700070925     C*
093800071029     C***  <===  -----------------------------------------
093900040510     C*
094000030715     C                   ENDIF
094100030715     C                   EVAL      i = i + 1
094200030715     C                   ENDDO
094300070925     C*
094400071029     C***  ===>  Gestione campi "particolari" fuori ciclo
094500130612     C*
094600070925     C*
094700071029     C***  <===  -----------------------------------------
094800070925     C*
094900040510     C*
095000130612     C* Considerazioni finali
095100130612     C*
095200130612     C                   ENDIF
095300020204     C*
095400010607     C                   ENDSR
095500010607     C*----------------------------------------------------*
095600010330
095700100318
095800100318
095900100318     C*----------------------------------------------------*
096000100318     C*  CONTROLLO NUMERICITA' CAMPI
096100100318     C*----------------------------------------------------*
096200100318     C     CHKNUM        BEGSR
096300100318     C*
096400100318     C                   IF        PiDecChr = *blanks
096500100318     C                   EVAL      PiDecChr = CharNUM
096600100318     C                   ENDIF
096700100318     C*
096800100318     C                   callp     UBISNUM_Check(PiStr
096900100318     C                                          :PiDecChr
097000100318     C                                          :PiVal
097100100318     C                                          :PiNum
097200100318     C                                          :PiInt)
097300100318     C*
097400100318     C                   ENDSR
097500010601     C***
097600010601
097700010601
097800010601
097900010601      /TITLE Invio dei dati al punto operativo.
098000010601     C     opeini        BEGSR
098100010601     C*
098200010601     C* Inizializzo flag e contatori operativi
098300010601     C                   movel     '0'           flgGiro           1
098400010601     C                   movel     '0'           flgMulti          1
098500010601     C                   movel     '1'           flgStato          1
098600010615     C                   movel     '0'           flgOk             1
098700010601     C                   z-add     *zeros        cntNonEl         10 0
098800010601     C                   z-add     *zeros        invfil            3 0
098900130612     C*
099000130612     C* Scrivo la riga di intestazione colonne del file di output
099100130612     C                   eval      wDati = 'SPED BRT;'+
099200130731     C                                     'DATA SPED BRT;'+
099300130612     C                                     'RIF_N MITT;'+
099400130612     C                                     'RIF_A MITT;'+
099500130612     C                                     'DESTINATARIO;'+
099600130612     C                                     'LOCALITA;'+
099700130612     C                                     'FATT CLIENTE;'+
099800130612     C                                     'FATT BRT;'+
099900130701     C                                     'STATO;'+
100000130813     C                                     'SQUADRATURA;'+
100100130813     C                                     'DATA FATTURA CHECK;'+
100200130827     C                                     'DATA FATTURA BRT;'+
100300130827     C                                     'PESO BOLLETTATO;'+
100400130827     C                                     'PESO RILEVATO;'
100500130813     C                   exsr      wriVGD
100600010601     C*
100700010601     C                   ENDSR
100800010601     C***
100900100318
101000100318
101100100318
101200100318     C     normCSV       BEGSR
101300100318     C*
101400100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
101500100318     C* separatori campi
101600100318     C                   if        CharTXT <> *blanks
101700100318     C                   z-add     *zeros        wLenVINDTA        4 0
101800100318     C                   z-add     *zeros        wFlgVINDTA        1 0
101900100318     C                   setoff                                       80
102000100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
102100100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
102200100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
102300100318     C                   if        not *in80
102400100318     C                   seton                                        80
102500100318     C                   else
102600100318     C                   setoff                                       80
102700100318     C                   endif
102800100318     C                   endif
102900100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
103000100318     C                             and *in80 = *on
103100100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
103200100318     C                   endif
103300100318     C                   enddo
103400100318     C                   endif
103500100318     C*
103600100318     C                   ENDSR
103700100318     C***
103800130612
103900130612
104000130612
104100130612     C     strTRUL47     BEGSR
104200130612     C*
104300130612     C                   movel     'N'           wProcedi          1
104400130612     C*
104500130612     C* Avvio il blocco elaborazione TIVGD x tipo file in questione
104600130612     C                   clear                   trul47ds
104700130612     C                   eval      d47opz  = 'I'
104800130612     C                   eval      d47tip  = inTIP
104900130612     C                   eval      d47lck  = 'N'
105000130612     C                   eval      d47chkj = 'S'
105100130612     C                   eval      d47pgm  = 'TITV2Q4R'
105200130612     C                   call      'TRUL47R'
105300130612     C                   parm                    trul47ds
105400130612     C*
105500130612     C* Se elaborazione consentita => proseguo
105600130612     C                   if        d47sts <> 'A'
105700130612     C                   movel     'S'           wProcedi
105800130612     C                   endif
105900130612     C*
106000130612     C                   ENDSR
106100130612
106200130612
106300130612
106400130612     C     endTRUL47     BEGSR
106500130612     C*
106600130612     C                   if        wProcedi = 'S'
106700130612     C*
106800130612     C* Elimino il blocco elaborazione TIVGD x tipo file in questione
106900130612     C                   clear                   trul47ds
107000130612     C                   eval      d47opz  = 'F'
107100130612     C                   eval      d47tip  = inTIP
107200130612     C                   call      'TRUL47R'
107300130612     C                   parm                    trul47ds
107400130612     C*
107500130612     C                   endif
107600130612     C*
107700130612     C                   ENDSR
107800130612
107900130612
108000130612
108100130612
108200130612      /TITLE Valorizzazione Progressivo Applicazione
108300130612     C     calprog       begsr
108400130612     C*
108500130612     C                   movel     *blanks       dwlisv            2
108600130612     C                   movel     *all'0'       dwlprg           10
108700130612     C                   z-add     *zeros        wrkprg            8 0
108800130612     C*
108900130612     C                   eval      dwlisv = inISV
109000130612     C*
109100130612     C                   open      tis7prgf
109200130612     C*
109300130612     C                   read(e)   tis7prgf
109400130612     C                   if        not %error
109500130612     C                   eval      dwlprg = f_tis7prgf
109600130612     C*
109700130612     C                   move(p)   dwlprg        wrkprg
109800130612     C                   add       1             wrkprg
109900130612     C                   move(p)   wrkprg        dwlprg
110000130612     C                   movel     dwlisv        dwlprg
110100130612     C*
110200130612     C                   eval      f_tis7prgf = dwlprg
110300130612     C                   update    tis7prg0
110400130612     C                   endif
110500130612     C*
110600130612     C                   close     tis7prgf
110700130612     C*
110800130612     C                   endsr
110900010330
111000100326
111100010330
111200000613     C     *inzsr        BEGSR
111300990910     C*
111400990910     C     *entry        plist
111500130612     C                   parm                    tivlrds
111600990921     C                   parm      wrkesito      esito
111700000724     C                   parm                    prmlit
111800000710     C                   parm                    prmfir
111900010330     C*
112000010330     C* CALCOLA LA DATA CORRENTE
112100100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
112200100318     C                   eval      datcor = %dec(%date() : *ISO)
112300130612     C*
112400130612     C* Definizione chiavi
112500130612     C*
112600130612     C* Chiave su TITAS32C - Completa
112700130612     C     KEYtas32C     KLIST
112800130612     C                   KFLD                    s_tasRMN
112900130612     C                   KFLD                    p_tasCCM
113000130702     C*
113100130702     C* Chiave su TITAS31C - Completa
113200130702     C     KEYtas31C     KLIST
113300130702     C                   KFLD                    keyKSC
113400130702     C                   KFLD                    keyDFT
113500130612     C*
113600130612     C* Chiave su TITA430C - Completa
113700130612     C     KEYta430      KLIST
113800130612     C                   KFLD                    tasAAS
113900130612     C                   KFLD                    tasLNP
114000130612     C                   KFLD                    tasNRS
114100130612     C                   KFLD                    tasNSP
114200130612     C                   KFLD                    ta4TRC
114300000613     C*
114400000613     C                   ENDSR
114500000613     C***
114600040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
114700130702INRMN;W_INAAS;INRSD;INLOD;W_INPOR;INRMA;INDFT
114800110421
114900110315
115000110315
115100100318
