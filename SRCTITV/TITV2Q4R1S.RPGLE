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
003300130612     D wStatus         s             15    inz
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
021200130612     C                   eval      s_tasAAS = w_tasAAS
021300130612     C                   eval      s_tasRMN = w_tasRMN
021400130612     C                   eval      s_tasRMA = w_tasRMA
021500130612     C                   eval      s_tasRSD = w_tasRSD
021600130612     C                   eval      s_tasLOD = w_tasLOD
021700130612     C                   else
021800130612     C                   if        s_tasRMN <> w_tasRMN
021900130612     C                   exsr      chkFAT
022000130612     C                   exsr      valVGD
022100130612     C                   exsr      wriVGD
022200130612     C                   eval      s_tasAAS = w_tasAAS
022300130612     C                   eval      s_tasRMN = w_tasRMN
022400130612     C                   eval      s_tasRMA = w_tasRMA
022500130612     C                   eval      s_tasRSD = w_tasRSD
022600130612     C                   eval      s_tasLOD = w_tasLOD
022700130612     C                   z-add     *zeros        wPortoTot
022800130612     C                   endif
022900130612     C                   endif
023000130612     C*
023100130612     C* Vado in sommatoria del porto cliente
023200130612     C                   eval      wPortoTot = wPortoTot + w_tasPOR
023300130612     C*
023400080513     C                   endif
023500010604     C*
023600130612     C                   if        *in31 = *off
023700010604     C                   eval      vinflg = '1'
023800010604     C                   else
023900010604     C                   eval      vinflg = '2'
024000010604     C                   endif
024100010601     C*
024200010604     C                   endif
024300010604     C*
024400010330     C                   else
024500010330     C                   eval      vinflg = '1'
024600010330     C                   endif
024700010601     C*
024800010601     C  N70              update    tivin000
024900010330     C*
025000030822     C  N70              enddo
025100010601     C*
025200130612     C* Scarico l'ultima rottura rimasta in canna
025300130612     C                   exsr      chkFAT
025400130612     C                   exsr      valVGD
025500130612     C                   exsr      wriVGD
025600130612     C*
025700130612     C* Imposto l'esito traduzione fisso a OK (errori già evidenti sul output)
025800010330     C                   eval      wrkesito = '0'
025900010330     C*
026000010330     C                   if        %open(tivin00r)
026100010330     C                   close     tivin00r
026200010330     C                   endif
026300010601     C*
026400010618     C                   if        flgGiro = '1'
026500010601     C                   exsr      endela
026600010618     C                   endif
026700010330     C*
026800010330     C                   ENDSR
026900010330     C***
027000130612
027100130612
027200130612
027300130612     C*----------------------------------------------------*
027400130612     C*  CONFRONTO CON BOLLA DI SEDE
027500130612     C*----------------------------------------------------*
027600130612     C     CHKFAT        BEGSR
027700130612     C*
027800130612     C* Aggancio la bolla in sede
027900130612     C                   movel     'N'           wOK               1
028000130612     C     KEYtas32C     setll     titas32c
028100130612     C                   if        %equal(titas32c)
028200130612     C     KEYtas32C     reade     titas32c
028300130612     C                   dow       not %eof(titas32c)
028400130612     C                   movel     'S'           wOK
028500130612     C*
028600130612     C* Se richiesti applico i filtri
028700130612     C*
028800130612     C                   if        s_tasAAS > *zeros
028900130612     C                   if        s_tasAAS = tasAAS
029000130612     C                   else
029100130612     C                   movel     'N'           wOK
029200130612     C                   endif
029300130612     C                   endif
029400130612     C*
029500130612     C                   if        p_tasCCM > *zeros
029600130612     C                   if        p_tasCCM = tasCCM
029700130612     C                   else
029800130612     C                   movel     'N'           wOK
029900130612     C                   endif
030000130612     C                   endif
030100130612     C*
030200130612     C                   if        p_tasLNP > *zeros
030300130612     C                   if        p_tasLNP = tasLNP
030400130612     C                   else
030500130612     C                   movel     'N'           wOK
030600130612     C                   endif
030700130612     C                   endif
030800130612     C*
030900130612     C                   if        p_tasNRS > *zeros
031000130612     C                   if        p_tasNRS = tasNRS
031100130612     C                   else
031200130612     C                   movel     'N'           wOK
031300130612     C                   endif
031400130612     C                   endif
031500130612     C*
031600130612     C                   if        p_tasCTR > *zeros
031700130612     C                   if        p_tasCTR = tasCTR
031800130612     C                   else
031900130612     C                   movel     'N'           wOK
032000130612     C                   endif
032100130612     C                   endif
032200130612     C*
032300130612     C                   if        p_tasTBL <> *blanks
032400130612     C                   if        p_tasTBL  = tasTBL
032500130612     C                   else
032600130612     C                   movel     'N'           wOK
032700130612     C                   endif
032800130612     C                   endif
032900130612     C*
033000130612     C                   if        p_tasTSP <> *blanks
033100130612     C                   if        p_tasTSP  = tasTSP
033200130612     C                   else
033300130612     C                   movel     'N'           wOK
033400130612     C                   endif
033500130612     C                   endif
033600130612     C*
033700130612     C                   if        p_tasCTM <> *blanks
033800130612     C                   if        p_tasCTM  = tasCTM
033900130612     C                   else
034000130612     C                   movel     'N'           wOK
034100130612     C                   endif
034200130612     C                   endif
034300130612     C*
034400130612     C                   if        p_tasFFD <> *blanks
034500130612     C                   if        p_tasFFD  = tasFFD
034600130612     C                   else
034700130612     C                   movel     'N'           wOK
034800130612     C                   endif
034900130612     C                   endif
035000130612     C*
035100130612     C* Aggancio il file estensione riferimenti bolle - tipo record 'A'
035200130612     C***                clear                   ta4NOT
035300130612     C***                eval      ta4TRC = 'A'
035400130612     C***  KEYta430      chain     tita430c
035500130612     C***                if        %found(tita430c)
035600130612     C***                if        s_tasRMA <> *blanks
035700130612     C***                if        s_tasRMA  = ta4NOT
035800130612     C***                else
035900130612     C***                movel     'N'           wOK
036000130612     C***                endif
036100130612     C***                endif
036200130612     C***                endif
036300130612     C*
036400130612     C* Se già trovato record perfettamente OK => esco dal ciclo
036500130612     C                   if        wOK = 'S'
036600130612     C                   leave
036700130612     C                   endif
036800130612     C*
036900130612     C     KEYtas32C     reade     titas32c
037000130612     C                   enddo
037100130612     C                   endif
037200130612     C*
037300130612     C* Considerazioni finali sul confronto fattura (solo su PORTO)
037400130612     C                   if        *in31
037500130612     C                   eval      wStatus = 'ERR_INPUT'
037600130612     C                   else
037700130612     C                   if        wOK <> 'S'
037800130612     C                   eval      wStatus = 'MANCA BRT'
037900130612     C                   else
038000130612     C                   z-add     tasPOR        wPorto
038100130612     C                   if        wPortoTot = wPorto
038200130612     C                   eval      wStatus = 'CONFORME'
038300130612     C                   else
038400130612     C                   eval      wStatus = 'DIFFERENZA'
038500130612     C                   endif
038600130612     C                   endif
038700130612     C                   endif
038800130612     C*
038900130612     C                   ENDSR
039000130612     C***
039100130702
039200130702
039300130702
039400130702     C*----------------------------------------------------*
039500130702     C*  LETTURA BOLLE FATTURATE CON INCLUSE IN LISTA CLIENTE
039600130702     C*----------------------------------------------------*
039700130702     C     RDTAS         BEGSR
039800130702     C*
039900130702     C                   eval      keyDFT = w_tasDFT
040000130702     C*
040100130702     C                   movel     'N'           wOK               1
040200130702     C     KEYtas31C     setll     titas31c
040300130702     C                   if        %equal(titas31c)
040400130702     C     KEYtas31C     reade     titas31c
040500130702     C                   dow       not %eof(titas31c)
040600130702     C                   clear                   titasds_s
040700130702     C                   clear                   s_tasRMA
040800130702     C                   clear                   wPortoTot
040900130702     C                   eval      wPorto = tasPOR
041000130702     C*
041100130702     C* Considero solamente se bolla ha un imponibile
041200130702     C                   if        tasIMV > *zeros
041300130702     C*
041400130702     C* Verifico se spedizione corrente già inclusa in elenco cleinte
041500130702     C                   if        %lookup(tasRMN:skRMN:1) = 0
041600130702     C* Se non già presente imposto status e scarico il buffer
041700130702     C                   eval      wStatus = 'MANCA CLI'
041800130702     C                   exsr      valVGD
041900130702     C                   exsr      wriVGD
042000130702     C                   endif
042100130702     C*
042200130702     C                   endif
042300130702     C*
042400130702     C     KEYtas31C     reade     titas31c
042500130702     C                   enddo
042600130702     C                   endif
042700130702     C*
042800130702     C                   ENDSR
042900130702     C***
043000040119
043100040119
043200040119
043300040119     C*----------------------------------------------------*
043400130612     C*  COMPOSIZIONE BUFFER RECORDS OUTPUT
043500040119     C*----------------------------------------------------*
043600130612     C     VALVGD        BEGSR
043700080513     C*
043800130612     C                   eval      wDati = %editc(tasAAS:'X')+'/'+
043900130612     C                                     %editc(tasLNP:'X')+'/'+
044000130612     C                                     %editc(tasNRS:'X')+'/'+
044100130612     C                                     %editc(tasNSP:'X')+';'+
044200130612     C                                     %editc(s_tasRMN:'X')+';'+
044300130612     C                                      %trim(s_tasRMA)+';'+
044400130612     C                                      %trim(s_tasRSD)+';'+
044500130612     C                                      %trim(s_tasLOD)+';'+
044600130612     C                                     %editc(wPortoTot:'3')+';'+
044700130612     C                                     %editc(wPorto:'3')+';'+
044800130701     C                                      %trim(wStatus)+';'+
044900130701     C                                    %editc(%abs(wPorto-wPortoTot):'3')+';'
045000130612     C*
045100040119     C                   ENDSR
045200130612
045300130612
045400130612
045500130612     C*----------------------------------------------------*
045600130612     C*  SCARICAMENTO BUFFER RECORDS OUTPUT
045700130612     C*----------------------------------------------------*
045800130612     C     WRIVGD        BEGSR
045900130612     C*
046000130612     C* Scarico il buffer di output
046100130612     C                   clear                   tivgd000
046200130612     C                   eval      vgdDTA = wDati
046300130612     C                   eval      vgdTIP = inTIP
046400130612     C                   eval      vgdKSU = inKSU
046500130612     C                   eval      vgdTSC = 'WW'
046600130612     C                   eval      vgdDAT = datcor
046700130612     C                   eval      vgdPGM = 'TITV2Q4R'
046800130612     C                   write     tivgd000
046900130702     C*
047000130702     C* Memorizzo il RMN corrente
047100130702     C                   add       1             idxRMN
047200130702     C                   eval      skRMN(idxRMN) = w_tasRMN
047300130612     C*
047400130612     C                   ENDSR
047500010601
047600010601
047700010601
047800010330     C*----------------------------------------------------*
047900030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
048000010330     C*----------------------------------------------------*
048100040119     C     INZVAR        BEGSR
048200030822     C*
048300030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
048400030822     C                   CLEAR                   SkSplitCSV
048500030822     C                   EVAL      i = 1
048600030822     C                   EVAL      posDa = *zeros
048700030822     C                   EVAL      posA  = *zeros
048800080527     C*
048900130612     C                   CLEAR                   titasds_w
049000130612     C                   CLEAR                   tivgd000
049100130612     C                   CLEAR                   wPorto
049200130612     C                   CLEAR                   wStatus
049300130612     C                   CLEAR                   wDati
049400030822     C*
049500030822     C* Reimposto i valori di default
049600030822     C                   EXSR      DEFCAM
049700030822     C*
049800010330     C                   ENDSR
049900010330     C*----------------------------------------------------*
050000030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
050100010330     C*----------------------------------------------------*
050200010330     C     DEFCAM        BEGSR
050300030822     C*
050400030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
050500030715     C* e delimitatore testo.
050600030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
050700030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
050800030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
050900100318     C*
051000100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
051100100318     C                   EVAL      CharSOS = CharNUM
051200030822     C*
051300030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
051400030822     C                   EVAL      posDaDft = 1
051500030822     C                   EVAL      posADft  = 0
051600030822     C                   EVAL      wGiroDft = 0
051700030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
051800030822     C                             posDaDft > 0
051900030822     C*
052000030822     C* Gestisco il 1° giro
052100030822     C                   IF        wGiroDft = 0
052200030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
052300030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
052400030822     C* Incremento il contatore dei "giri"
052500030822     C                   EVAL      wGiroDft = 1
052600030822     C                   ELSE
052700030822     C                   EVAL      posDaDft = posADft
052800030822     C                   ENDIF
052900030822     C* Eseguo lo scan x trovare la fine del campo corrente
053000030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
053100030822     C*
053200030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
053300030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
053400030822     C                   IF        posDaDft > 0 AND
053500030822     C                             posADft  > 0
053600130612     C* CCM
053700030822     C                   IF        %subst(
053800030822     C                             %subst(vlrppt:posDaDft+1:
053900030822     C                             posADft-posDaDft-1):1:3)
054000030822     C                             = 'CCM'
054100030822     C                   EVAL      PiStr=%trim(%subst(
054200030822     C                             %subst(vlrppt:posDaDft+1:
054300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054400030822     C                   EXSR      CHKNUM
054500030822     C                   IF        PiInt=*on
054600130612     C                   Z-ADD     PiVal         p_tasCCM
054700030822     C                   ENDIF
054800030822     C                   ENDIF
054900030822     C* LNP
055000030822     C                   IF        %subst(
055100030822     C                             %subst(vlrppt:posDaDft+1:
055200030822     C                             posADft-posDaDft-1):1:3)
055300030822     C                             = 'LNP'
055400030822     C                   EVAL      PiStr=%trim(%subst(
055500030822     C                             %subst(vlrppt:posDaDft+1:
055600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055700030822     C                   EXSR      CHKNUM
055800030822     C                   IF        PiInt=*on
055900130612     C                   Z-ADD     PiVal         p_tasLNP
056000030822     C                   ENDIF
056100030822     C                   ENDIF
056200030822     C* NRS
056300030822     C                   IF        %subst(
056400030822     C                             %subst(vlrppt:posDaDft+1:
056500030822     C                             posADft-posDaDft-1):1:3)
056600030822     C                             = 'NRS'
056700030822     C                   EVAL      PiStr=%trim(%subst(
056800030822     C                             %subst(vlrppt:posDaDft+1:
056900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057000030822     C                   EXSR      CHKNUM
057100030822     C                   IF        PiInt=*on
057200130612     C                   Z-ADD     PiVal         p_tasNRS
057300030822     C                   ENDIF
057400030822     C                   ENDIF
057500030822     C* CTR
057600030822     C                   IF        %subst(
057700030822     C                             %subst(vlrppt:posDaDft+1:
057800030822     C                             posADft-posDaDft-1):1:3)
057900030822     C                             = 'CTR'
058000030822     C                   EVAL      PiStr=%trim(%subst(
058100030822     C                             %subst(vlrppt:posDaDft+1:
058200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058300030822     C                   EXSR      CHKNUM
058400030822     C                   IF        PiInt=*on
058500130612     C                   Z-ADD     PiVal         p_tasCTR
058600030822     C                   ENDIF
058700030822     C                   ENDIF
058800130612     C* TBL
058900030822     C                   IF        %subst(
059000030822     C                             %subst(vlrppt:posDaDft+1:
059100030822     C                             posADft-posDaDft-1):1:3)
059200130612     C                             = 'TBL'
059300130612     C                   EVAL      p_tasTBL=%trim(%subst(
059400030822     C                             %subst(vlrppt:posDaDft+1:
059500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059600030822     C                   ENDIF
059700030822     C* TSP
059800030822     C                   IF        %subst(
059900030822     C                             %subst(vlrppt:posDaDft+1:
060000030822     C                             posADft-posDaDft-1):1:3)
060100030822     C                             = 'TSP'
060200130612     C                   EVAL      p_tasTSP=%trim(%subst(
060300030822     C                             %subst(vlrppt:posDaDft+1:
060400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060500030822     C                   ENDIF
060600030822     C* CTM
060700030822     C                   IF        %subst(
060800030822     C                             %subst(vlrppt:posDaDft+1:
060900030822     C                             posADft-posDaDft-1):1:3)
061000030822     C                             = 'CTM'
061100130612     C                   EVAL      p_tasCTM=%trim(%subst(
061200030822     C                             %subst(vlrppt:posDaDft+1:
061300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061400030822     C                   ENDIF
061500030822     C* FFD
061600030822     C                   IF        %subst(
061700030822     C                             %subst(vlrppt:posDaDft+1:
061800030822     C                             posADft-posDaDft-1):1:3)
061900030822     C                             = 'FFD'
062000130612     C                   EVAL      p_tasFFD=%trim(%subst(
062100030822     C                             %subst(vlrppt:posDaDft+1:
062200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062300030822     C                   ENDIF
062400130612     C* inKSU
062500130612     C                   IF        %subst(
062600130612     C                             %subst(vlrppt:posDaDft+1:
062700130612     C                             posADft-posDaDft-1):1:3)
062800130612     C                             = 'KSU'
062900130612     C                   EVAL      inKSU=%trim(%subst(
063000130612     C                             %subst(vlrppt:posDaDft+1:
063100130612     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063200130612     C                   ENDIF
063300130612     C* inTIP
063400130612     C                   IF        %subst(
063500130612     C                             %subst(vlrppt:posDaDft+1:
063600130612     C                             posADft-posDaDft-1):1:3)
063700130612     C                             = 'TIP'
063800130612     C                   EVAL      inTIP=%trim(%subst(
063900130612     C                             %subst(vlrppt:posDaDft+1:
064000130612     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064100130612     C                   ENDIF
064200130612     C* inISV
064300130612     C                   IF        %subst(
064400130612     C                             %subst(vlrppt:posDaDft+1:
064500130612     C                             posADft-posDaDft-1):1:3)
064600130612     C                             = 'ISV'
064700130612     C                   EVAL      inISV=%trim(%subst(
064800130612     C                             %subst(vlrppt:posDaDft+1:
064900130612     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065000130612     C                   ENDIF
065100030822     C* ...
065200030822     C                   ENDIF
065300030822     C                   ENDDO
065400020204     C*
065500010330     C                   ENDSR
065600010607     C*----------------------------------------------------*
065700021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
065800010607     C*----------------------------------------------------*
065900130612     C     IMPFILE       BEGSR
066000010607     C*
066100130612     C                   SETOFF                                       31
066200010607     C*
066300030715     C**********
066400030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
066500030715     C**********
066600030715     C                   IF        wGiro = *zeros
066700040113     C*
066800040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
066900040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
067000080429     C                             %trim(intcol(3))+%trim(intcol(4))+
067100080429     C                             %trim(intcol(5))<>*blanks
067200040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
067300080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
067400080429     C                                      %trim(intcol(5))
067500070823     C     ';':CharCSV   XLATE     vindta        vindta
067600040510     C                   ENDIF
067700031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
067800031201     C     minu:maiu     XLATE     vindta        vindta
067900030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
068000030715     C                   DOW       posDa <= %len(%trim(vindta))
068100030715     C*
068200030715     C* Gestisco il 1° campo
068300030715     C                   IF        i = 1
068400030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
068500030715     C                   EVAL      posDa = 1
068600030715     C* Eseguo lo scan x trovare la fine del primo campo
068700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
068800030715     C* A questo punto "estraggo" il campo corrente
068900030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
069000030715     C                                                    (posA-posDa))
069100030715     C* X i campi successivi al 1°
069200030715     C                   ELSE
069300030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
069400030715     C                   EVAL      posDa = posA + 1
069500030715     C* Eseguo lo scan x trovare la fine del campo corrente
069600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
069700030715     C* Gestisco l'ultimo campo
069800030715     C                   IF        posA = *zeros
069900030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
070000030715     C* A questo punto "estraggo" il campo corrente
070100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
070200030715     C                                                    (posA-posDa))
070300030715     C                   LEAVE
070400030715     C                   ELSE
070500030715     C* A questo punto "estraggo" il campo corrente
070600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
070700030715     C                                                    (posA-posDa))
070800030715     C                   ENDIF
070900030715     C                   ENDIF
071000030715     C* Incremento il contatore d campo
071100030715     C                   EVAL      i = i +1
071200030715     C                   ENDDO
071300030715     C*
071400030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
071500030715     C* delimitazione testo
071600030715     C                   EVAL      i = 1
071700030715     C                   DOW       i < %elem(SkSplitFLD)
071800030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
071900030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
072000030715     C                   EVAL      i = i + 1
072100030715     C                   ENDDO
072200030715     C                   EVAL      wGiro = 1
072300030715     C                   ELSE
072400030715     C                   EVAL      wGiro = 2
072500031204     C**********
072600031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
072700031204     C* d separatore campo
072800031204     C**********
072900031204     C                   EVAL      vindta = %trim(vindta)
073000031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
073100031204     C                   EVAL      vindta = %subst(vindta:2)
073200031204     C                   ENDDO
073300031204     C*
073400031204     C                   Z-ADD     *zeros        lunghInput        4 0
073500031204     C                   EVAL      lunghInput = %len(%trim(vindta))
073600031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
073700031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
073800031204     C                   EVAL      lunghInput = %len(%trim(vindta))
073900031204     C                   ENDDO
074000030715     C**********
074100030715     C* Effettuo lo split del campo dati d input
074200030715     C**********
074300030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
074400030715     C                   DOW       posDa <= %len(%trim(vindta))
074500030715     C*
074600030715     C* Gestisco il 1° campo
074700030715     C                   IF        i = 1
074800030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
074900030715     C                   EVAL      posDa = 1
075000030715     C* Eseguo lo scan x trovare la fine del primo campo
075100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
075200030715     C* A questo punto "estraggo" il campo corrente
075300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
075400030715     C                                                    (posA-posDa))
075500030715     C* X i campi successivi al 1°
075600030715     C                   ELSE
075700030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
075800030715     C                   EVAL      posDa = posA + 1
075900031204     C* Verifico che nn vi sia il campo nn valorizzato
076000031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
076100031204     C* Se campo nn valorizzato skippo al prossimo
076200031204     C                   EVAL      SkSplitCSV(i) = *blanks
076300031204     C                   EVAL      posA  = posA + 1
076400031204     C                   ELSE
076500030715     C* Eseguo lo scan x trovare la fine del campo corrente
076600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
076700030715     C* Gestisco l'ultimo campo
076800030715     C                   IF        posA = *zeros
076900030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
077000030715     C* A questo punto "estraggo" il campo corrente
077100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
077200030715     C                                                    (posA-posDa))
077300030715     C                   LEAVE
077400030715     C                   ELSE
077500030715     C* A questo punto "estraggo" il campo corrente
077600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
077700030715     C                                                    (posA-posDa))
077800030715     C                   ENDIF
077900031204     C                   ENDIF
078000030715     C                   ENDIF
078100030715     C* Incremento il contatore d campo
078200030715     C                   EVAL      i = i +1
078300030715     C                   ENDDO
078400030715     C*
078500030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
078600030715     C* delimitazione testo
078700030715     C                   EVAL      i = 1
078800030715     C                   DOW       i < %elem(SkSplitCSV)
078900030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
079000030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
079100030715     C                   EVAL      i = i + 1
079200030715     C                   ENDDO
079300100326     C*
079400030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
079500030715     C                   EVAL      i = 1
079600030715     C                   DOW       i < %elem(SkSplitFLD)
079700030715     C*
079800030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
079900030715     C                   IF        SkSplitFLD(i) <> *blanks AND
080000030715     C                             SkSplitCSV(i) <> *blanks
080100070823     C*
080200130612 xxx C***  ===>  Gestione campi "particolari" dentro ciclo
080300110421     C*
080400130612     C*** RMN
080500130612     C                   IF        %trim(SkSplitFLD(i)) = 'INRMN'
080600130612     C                   EVAL      PiStr=SkSplitCSV(i)
080700130612     C                   EXSR      CHKNUM
080800130612     C                   IF        PiInt=*on
080900130612     C                   Z-ADD     PiVal         w_tasRMN
081000130612     C                   ELSE
081100130612     C                   SETON                                        31
081200130612     C                   EVAL      w_tasRMN = *zeros
081300130612     C                   EVAL      vinmsg = %trimr(vinmsg)
081400130612     C                             + ' ' + 'INRMN'
081500130612     C                   ENDIF
081600130612     C                   ENDIF
081700130612     C*** AAS
081800130612     C                   IF        %trim(SkSplitFLD(i)) = 'W_INAAS'
081900130612     C                   EVAL      PiStr=SkSplitCSV(i)
082000130612     C                   EXSR      CHKNUM
082100130612     C                   IF        PiInt=*on
082200130612     C                   Z-ADD     PiVal         wDATA             8 0
082300130612     C                   MOVEL     wDATA         w_tasAAS
082400130612     C                   ELSE
082500130612     C                   SETON                                        31
082600130612     C                   EVAL      w_tasAAS = *zeros
082700130612     C                   EVAL      vinmsg = %trimr(vinmsg)
082800130612     C                             + ' ' + 'INAAS'
082900130612     C                   ENDIF
083000130612     C                   ENDIF
083100130612     C*** POR
083200130612     C                   IF        %trim(SkSplitFLD(i)) = 'W_INPOR'
083300130612     C*
083400130612     C* Effettuo la chiamata al *pgm d "bonifica" x il campo 'W_INPOR'
083500130612     C                   EVAL      TxtInOut   = SkSplitCSV(i)
083600130612     C                   EVAL      ElencoChar = '1234567890,.'
083700130612     C                   EVAL      EsitoBon   = *blanks
083800130612     C*
083900130612     C                   CALL      'XCHKCHAR'
084000130612     C                   PARM                    TxtInOut
084100130612     C                   PARM                    ElencoChar
084200130612     C                   PARM                    TipoElenco
084300130612     C                   PARM                    CharSost
084400130612     C                   PARM                    UpperCase
084500130612     C                   PARM                    ChkNull
084600130612     C                   PARM                    CharNull
084700130612     C                   PARM                    EsitoBon
084800130612     C*
084900130612     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
085000130612     C                   IF        EsitoBon = '1'
085100130612     C                   EVAL      SkSplitCSV(i) = TxtInOut
085200130612     C                   ENDIF
085300130612     C*
085400130612     C                   EVAL      PiStr=SkSplitCSV(i)
085500130612     C                   EXSR      CHKNUM
085600130612     C                   IF        PiNum=*on
085700130612     C                   Z-ADD     PiVal         w_tasPOR
085800130612     C                   ELSE
085900130612     C                   SETON                                        31
086000130612     C                   EVAL      w_tasPOR = *zeros
086100130612     C                   EVAL      vinmsg = %trimr(vinmsg)
086200130612     C                             + ' ' + 'INPOR'
086300130612     C                   ENDIF
086400130612     C                   ENDIF
086500130612     C*** RSD
086600130612     C                   IF        %trim(SkSplitFLD(i)) = 'INRSD'
086700130612     C                   EVAL      w_tasRSD = SkSplitCSV(i)
086800110421     C                   ENDIF
086900130612     C*** LOD
087000130612     C                   IF        %trim(SkSplitFLD(i)) = 'INLOD'
087100130612     C                   EVAL      w_tasLOD = SkSplitCSV(i)
087200130612     C                   ENDIF
087300130612     C*** RMA
087400130612     C                   IF        %trim(SkSplitFLD(i)) = 'INRMA'
087500130612     C                   EVAL      w_tasRMA = SkSplitCSV(i)
087600130612     C                   ENDIF
087700130702     C*** DFT
087800130702     C                   IF        %trim(SkSplitFLD(i)) = 'INDFT'
087900130702     C                   EVAL      PiStr=SkSplitCSV(i)
088000130702     C                   EXSR      CHKNUM
088100130702     C                   IF        PiInt=*on
088200130702     C                   Z-ADD     PiVal         w_tasDFT
088300130702     C                   ELSE
088400130702     C                   SETON                                        31
088500130702     C                   EVAL      w_tasDFT = *zeros
088600130702     C                   EVAL      vinmsg = %trimr(vinmsg)
088700130702     C                             + ' ' + 'INDFT'
088800130702     C                   ENDIF
088900130702     C                   ENDIF
089000070925     C*
089100071029     C***  <===  -----------------------------------------
089200040510     C*
089300030715     C                   ENDIF
089400030715     C                   EVAL      i = i + 1
089500030715     C                   ENDDO
089600070925     C*
089700071029     C***  ===>  Gestione campi "particolari" fuori ciclo
089800130612     C*
089900070925     C*
090000071029     C***  <===  -----------------------------------------
090100070925     C*
090200040510     C*
090300130612     C* Considerazioni finali
090400130612     C*
090500130612     C                   ENDIF
090600020204     C*
090700010607     C                   ENDSR
090800010607     C*----------------------------------------------------*
090900010330
091000100318
091100100318
091200100318     C*----------------------------------------------------*
091300100318     C*  CONTROLLO NUMERICITA' CAMPI
091400100318     C*----------------------------------------------------*
091500100318     C     CHKNUM        BEGSR
091600100318     C*
091700100318     C                   IF        PiDecChr = *blanks
091800100318     C                   EVAL      PiDecChr = CharNUM
091900100318     C                   ENDIF
092000100318     C*
092100100318     C                   callp     UBISNUM_Check(PiStr
092200100318     C                                          :PiDecChr
092300100318     C                                          :PiVal
092400100318     C                                          :PiNum
092500100318     C                                          :PiInt)
092600100318     C*
092700100318     C                   ENDSR
092800010601     C***
092900010601
093000010601
093100010601
093200010601      /TITLE Invio dei dati al punto operativo.
093300010601     C     opeini        BEGSR
093400010601     C*
093500010601     C* Inizializzo flag e contatori operativi
093600010601     C                   movel     '0'           flgGiro           1
093700010601     C                   movel     '0'           flgMulti          1
093800010601     C                   movel     '1'           flgStato          1
093900010615     C                   movel     '0'           flgOk             1
094000010601     C                   z-add     *zeros        cntNonEl         10 0
094100010601     C                   z-add     *zeros        invfil            3 0
094200130612     C*
094300130612     C* Scrivo la riga di intestazione colonne del file di output
094400130612     C                   eval      wDati = 'SPED BRT;'+
094500130612     C                                     'RIF_N MITT;'+
094600130612     C                                     'RIF_A MITT;'+
094700130612     C                                     'DESTINATARIO;'+
094800130612     C                                     'LOCALITA;'+
094900130612     C                                     'FATT CLIENTE;'+
095000130612     C                                     'FATT BRT;'+
095100130701     C                                     'STATO;'+
095200130701     C                                     'SQUADRATURA;'
095300130612     C                   exsr      wriVGD
095400010601     C*
095500010601     C                   ENDSR
095600010601     C***
095700100318
095800100318
095900100318
096000100318     C     normCSV       BEGSR
096100100318     C*
096200100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
096300100318     C* separatori campi
096400100318     C                   if        CharTXT <> *blanks
096500100318     C                   z-add     *zeros        wLenVINDTA        4 0
096600100318     C                   z-add     *zeros        wFlgVINDTA        1 0
096700100318     C                   setoff                                       80
096800100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
096900100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
097000100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
097100100318     C                   if        not *in80
097200100318     C                   seton                                        80
097300100318     C                   else
097400100318     C                   setoff                                       80
097500100318     C                   endif
097600100318     C                   endif
097700100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
097800100318     C                             and *in80 = *on
097900100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
098000100318     C                   endif
098100100318     C                   enddo
098200100318     C                   endif
098300100318     C*
098400100318     C                   ENDSR
098500100318     C***
098600130612
098700130612
098800130612
098900130612     C     strTRUL47     BEGSR
099000130612     C*
099100130612     C                   movel     'N'           wProcedi          1
099200130612     C*
099300130612     C* Avvio il blocco elaborazione TIVGD x tipo file in questione
099400130612     C                   clear                   trul47ds
099500130612     C                   eval      d47opz  = 'I'
099600130612     C                   eval      d47tip  = inTIP
099700130612     C                   eval      d47lck  = 'N'
099800130612     C                   eval      d47chkj = 'S'
099900130612     C                   eval      d47pgm  = 'TITV2Q4R'
100000130612     C                   call      'TRUL47R'
100100130612     C                   parm                    trul47ds
100200130612     C*
100300130612     C* Se elaborazione consentita => proseguo
100400130612     C                   if        d47sts <> 'A'
100500130612     C                   movel     'S'           wProcedi
100600130612     C                   endif
100700130612     C*
100800130612     C                   ENDSR
100900130612
101000130612
101100130612
101200130612     C     endTRUL47     BEGSR
101300130612     C*
101400130612     C                   if        wProcedi = 'S'
101500130612     C*
101600130612     C* Elimino il blocco elaborazione TIVGD x tipo file in questione
101700130612     C                   clear                   trul47ds
101800130612     C                   eval      d47opz  = 'F'
101900130612     C                   eval      d47tip  = inTIP
102000130612     C                   call      'TRUL47R'
102100130612     C                   parm                    trul47ds
102200130612     C*
102300130612     C                   endif
102400130612     C*
102500130612     C                   ENDSR
102600130612
102700130612
102800130612
102900130612
103000130612      /TITLE Valorizzazione Progressivo Applicazione
103100130612     C     calprog       begsr
103200130612     C*
103300130612     C                   movel     *blanks       dwlisv            2
103400130612     C                   movel     *all'0'       dwlprg           10
103500130612     C                   z-add     *zeros        wrkprg            8 0
103600130612     C*
103700130612     C                   eval      dwlisv = inISV
103800130612     C*
103900130612     C                   open      tis7prgf
104000130612     C*
104100130612     C                   read(e)   tis7prgf
104200130612     C                   if        not %error
104300130612     C                   eval      dwlprg = f_tis7prgf
104400130612     C*
104500130612     C                   move(p)   dwlprg        wrkprg
104600130612     C                   add       1             wrkprg
104700130612     C                   move(p)   wrkprg        dwlprg
104800130612     C                   movel     dwlisv        dwlprg
104900130612     C*
105000130612     C                   eval      f_tis7prgf = dwlprg
105100130612     C                   update    tis7prg0
105200130612     C                   endif
105300130612     C*
105400130612     C                   close     tis7prgf
105500130612     C*
105600130612     C                   endsr
105700010330
105800100326
105900010330
106000000613     C     *inzsr        BEGSR
106100990910     C*
106200990910     C     *entry        plist
106300130612     C                   parm                    tivlrds
106400990921     C                   parm      wrkesito      esito
106500000724     C                   parm                    prmlit
106600000710     C                   parm                    prmfir
106700010330     C*
106800010330     C* CALCOLA LA DATA CORRENTE
106900100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
107000100318     C                   eval      datcor = %dec(%date() : *ISO)
107100130612     C*
107200130612     C* Definizione chiavi
107300130612     C*
107400130612     C* Chiave su TITAS32C - Completa
107500130612     C     KEYtas32C     KLIST
107600130612     C                   KFLD                    s_tasRMN
107700130612     C                   KFLD                    p_tasCCM
107800130702     C*
107900130702     C* Chiave su TITAS31C - Completa
108000130702     C     KEYtas31C     KLIST
108100130702     C                   KFLD                    keyKSC
108200130702     C                   KFLD                    keyDFT
108300130612     C*
108400130612     C* Chiave su TITA430C - Completa
108500130612     C     KEYta430      KLIST
108600130612     C                   KFLD                    tasAAS
108700130612     C                   KFLD                    tasLNP
108800130612     C                   KFLD                    tasNRS
108900130612     C                   KFLD                    tasNSP
109000130612     C                   KFLD                    ta4TRC
109100000613     C*
109200000613     C                   ENDSR
109300000613     C***
109400040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
109500130702INRMN;W_INAAS;INRSD;INLOD;W_INPOR;INRMA;INDFT
109600110421
109700110315
109800110315
109900100318
