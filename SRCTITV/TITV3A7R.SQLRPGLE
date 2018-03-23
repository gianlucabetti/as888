000100030715      /TITLE Upload via Internet: traduzione in FIVABWWR.
000200150420     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300150420     H DECEDIT('0,') DATEDIT(*DMY.)
000400000313     F*
000500990910     Ftivin00r  uF   E             DISK    usropn
000600140114     FTIVGD00F  O    E             DISK
000700160906     Ftis7prgf  uF   E             DISK    rename(tis7prgf:tis7prg0)
000800160906     F                                     prefix(f_)
000900160908     FFIVABwwr  O    E             DISK    usropn
001000160908     FFIVATwwr  O    E             DISK    usropn
001100000313     D*
001200160920     D*------------
001300160920     D* SCHIEDE A PROGRAMMA
001400160920     D*------------
001500160920     D intcol          S            100    DIM(5) CTDATA PERRCD(1)
001600010330     D*----------------------------------------------------
001700010330     D* DICHIARAZIOINE VARIABILI DI WRK
001800010330     D*----------------------------------------------------
001900160908     D dscmz         e ds                  inz
002000010330     D psds           sds
002100010330     D  procname         *PROC
002200010330     D tivlrds       e ds                  extname(tivlr00f)
002300140114     D tisi95ds      e ds
002400010330     D esito           s              1
002500010330     D prmlit          s             10
002600010330     D prmfir          s             10
002700150420     D pptCHR          s              1    inz
002800150420     D pptCSV          s              1    inz
002900160902     D wBufferOut      s           2048    inz
003000160902     D wVATNOT_A       s             35    inz
003100160902     D wVATNOT_B       s             35    inz
003200160902     D wVATNOT_E       s             35    inz
003300150420     D wVATNOT_I       s             70    inz
003400160908     D wVATNOT_J       s             70    inz
003500150420     D wVATNOT_S       s             35    inz
003600160902     D wVABNOT         s             35    inz
003700160906     D wVABCTR_E       s                   like(VABCTR) inz
003800150420     D wrkesito        s                   like(esito)
003900160902     D wVGD_Head       s              1  0 inz(*zeros)
004000160902     D wFlgCAS         s              1  0 inz(*zeros)
004100160902     D s_vindta        s                   like(vindta) inz(*blanks)
004200160908     D parccm          s              8    inz(*blanks)
004300160908     D parmbr          s             10    inz(*blanks)
004400160908     D paropz          s              1    inz(*blanks)
004500160908     D chkcall         s              1    inz(*blanks)
004600160909     D depSPE          s              7  0 inz
004700160902
004800160908
004900160908     D*------------------
005000160908     D* DS REPERIMENTO NUMERATORE
005100160908     D*------------------
005200160908     D trul33ds      e ds                  inz
005300160908     D*------------------
005400160908     D* DS ARCHITETTURA
005500160908     D*------------------
005600160908     D KPJBA         e ds                  inz
005700160908
005800160902
005900160902     D*------------------
006000160902     D* VARIABILI X LA CHIAMATA AL *PGM UTILITA' "XSPLIT"
006100160902     D*------------------
006200160902     D InStringa       S          65535A   VARYING                              (stringa input)
006300160902     D InSepara        S             10A                                        (separatore)
006400160902     D InTypeOut       S              1A                                        (tipo output)
006500160902     D wSkParam        S          65535A                                        (output)
006600160902     D OutErrore       S              1A                                        (flag errore)
006700160902     D OutMsg          S             80A                                        (messaggio errore)
006800160902     D SkSplitCSV_5    S            256    DIM(256)
006900160902
007000160902
007100030715     D*------------------
007200030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
007300030715     D*------------------
007400160902     D SkSplitFLD      S                   LIKE(SkSplitCSV_5) DIM(256)
007500160902     D SkSplitCSV      S                   LIKE(SkSplitCSV_5) DIM(256)
007600160902     D CharCSV         S              1
007700160902     D CharTXT         S              1
007800160902     D CharNUM         S              1
007900160902     D CharSOS         S              1
008000160902     D i               s              3  0 INZ(1)
008100030715     D wGiro           s              1  0 INZ(*zeros)
008200030822     D*------------------
008300030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
008400030822     D*------------------
008500160902     D posDaDft        S              3  0 INZ(*zeros)
008600160902     D posADft         S              3  0 INZ(*zeros)
008700160902     D wGiroDft        s              1  0 INZ(*zeros)
008800160908
008900160908
009000031201     D*------------------
009100031201     D* Costanti
009200031201     D*------------------
009300160902     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
009400160908     D MAIU            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
009500150420
009600010330
009700010330
009800990921     C                   reset                   esito
009900990921     C                   reset                   wrkesito
010000010601     C*
010100160908     C                   exsr      defcam
010200160908     C   50              exsr      calprog
010300150420     C                   exsr      rwfile
010400160908     C   50              exsr      strvgd
010500160906     C                   exsr      endela
010600160906     C*
010700010601     C                   seton                                        lr
010800160906
010900160906
011000160906     C*--------------------------------------------------------
011100160906     C* STRVGD - "LIBERO" LA TRANSAZIONE (GRUPPO) CORRENTE    *
011200160906     C*--------------------------------------------------------
011300160906     C     STRVGD        BEGSR
011400160906     C*
011500160906     C
011600170119     C/EXEC SQL
011700170227     C+ UPDATE TIVGD00F SET VGDSTO = 'R'
011800170119     C+ WHERE VGDSTO = '?'     AND VGDPRG = :VGDPRG AND
011900170119     C+       VGDTIP = :VGDTIP AND VGDKSU = :VGDKSU AND
012000170119     C+       VGDTSC = :VGDTSC
012100170119     C/END-EXEC
012200160906     C
012300160906     C*
012400160906     C                   ENDSR
012500160906     C***
012600160902
012700010601
012800010601     C*--------------------------------------------------------
012900160902     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
013000010601     C*--------------------------------------------------------
013100010601     C     ENDELA        BEGSR
013200080222     C*
013300080222     C* Effettuo lancio TISI95 solo x chiusura
013400080222     C                   CLEAR                   TISI95DS
013500080222     C                   EVAL      I95TLA = 'C'
013600080222     C                   CALL      'TISI95R'
013700080222     C                   PARM                    TISI95DS
013800160908     C*
013900160908     C* Effettuo la chiamata al CLLE preposto
014000160908     C  N50              call(e)   'TITVVTC'
014100160908     C                   parm                    parccm
014200160908     C                   parm                    parmbr
014300160908     C                   parm      '2'           paropz
014400000613     C*
014500010601     C                   ENDSR
014600010601     C***
014700010601
014800010601
014900010330     C*--------------------------------------------------------
015000150420     C* RWFILE  LEGGE TIVIN00R E SCRIVE FIVABWWF              *
015100010330     C*--------------------------------------------------------
015200150420     C     RWFILE        BEGSR
015300010330     C*
015400010330     C                   if        not %open(tivin00r)
015500010330     C                   open      tivin00r
015600010330     C                   endif
015700160908     C*
015800160908     C* Eseguo operazioni di aggiunta nuovo membro file FNVAT
015900160908     C  N50              exsr      prevat
016000160908     C   50              eval      chkcall = '0'
016100160908     C*
016200160908     C                   if        chkcall = '0'
016300160908     C*
016400160908     C                   if        not %open(fivabwwr)
016500160908     C                   open      fivabwwr
016600160908     C                   endif
016700160908     C*
016800160908     C                   if        not %open(fivatwwr)
016900160908     C                   open      fivatwwr
017000160908     C                   endif
017100010330     C*
017200160908     C                   clear                   §CTROKVB          7 0
017300160908     C                   clear                   §CTROKVT          7 0
017400160908     C                   clear                   §CTRMO            7 0
017500160908     C                   clear                   §CTRNO            7 0
017600160920     C*
017700160920     C* Se richiesto eseguo primo giro x by-pasare la mancanza della 1° riga con le
017800160920     C* intestazioni colonne nel file del cliente (che contiene quindi solo le righe di dati)
017900160920     C   60              exsr      addint
018000160920     C   60              exsr      inzvar
018100160920     C   60              exsr      impvab
018200010330     C*
018300030822     C                   do        *HIVAL
018400010330     C*
018500030822     C                   read      tivin00r                               70
018600160902     C*
018700160902     C* Salvo il buffer orginale ricevuto dal cliente
018800160902     C                   eval      s_vindta = vindta
018900160902     C*
019000160902     C                   if        *in70 = *off and
019100160902     C                             vindta <> *blanks and
019200160902     C                             (vinflg = *blanks
019300160902     C                              or vinflg = '0'
019400160902     C                              or vinflg = '2')
019500160902     C*
019600160902     C* Controlli di "base" sui dati grezzi
019700160902     C                   exsr      normCSV
019800040510     C*
019900160902     C                   if        wFlgVINDTA = 1
020000010330     C*
020100010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
020200010711     C                   if        vinflg = *blanks or vinflg = '0'
020300010711     C                   clear                   vinmsg
020400010711     C                   endif
020500160902     C*
020600160902 xxx C* Porto eventualmente da minuscolo in maiuscolo le INTESTAZIONI colonne cliente
020700160908 xxx C***  minu:MAIU     XLATE     vindta        vindta
020800160902     C*
020900160902     C* Splitto i dati
021000160902     C                   CLEAR                   SkSplitCSV_5
021100160902     C                   EVAL      InStringa = vindta
021200160902     C                   EVAL      InSepara  = CharCSV
021300160902     C                   EXSR      SR_SPLIT
021400160902     C*
021500160902     C*****************************
021600160916     C* *PERS (consigliato test almeno su 3 colonne)
021700160916     C* A questo punto verifico che le colonne mandatorie siano valorizzate correttamente
021800160920     C                   if        %len(%trim(SkSplitCSV_5(1)))  > 0 and
021900160920     C                             %len(%trim(SkSplitCSV_5(2)))  > 0 and
022000160920     C                             %len(%trim(SkSplitCSV_5(4)))  > 0 and
022100160920     C                             %len(%trim(SkSplitCSV_5(14))) > 0
022200160902     C*****************************
022300010601     C*
022400160902     C* Se OK => procedo con elaborazione
022500010330     C                   exsr      inzvar
022600010605     C                   exsr      impvab
022700010601     C*
022800010604     C* Ebbene...
022900160908     C                   if        wGiro > 1
023000160908     C  N31              add       1             §CTROKVB
023100160908     C   32              add       1             §CTRMO
023200160908     C   31              add       1             §CTRNO
023300160908     C*
023400160902     C   50              exsr      wriVAB_VGD
023500160908     C  N50              exsr      wriVAB_WW
023600150420     C                   endif
023700010604     C*
023800010604     C                   if        *in31 = *off and
023900010604     C                             *in32 = *off
024000010604     C                   eval      vinflg = '1'
024100010604     C                   else
024200010604     C                   eval      vinflg = '2'
024300010604     C                   endif
024400010601     C*
024500160902     C                   else
024600160902     C                   eval      vinflg = '1'
024700010604     C                   endif
024800160902     C*
024900160902     C                   else
025000160902     C                   eval      vinflg = '1'
025100160902     C                   endif
025200010604     C*
025300010330     C                   else
025400010330     C                   eval      vinflg = '1'
025500010330     C                   endif
025600010601     C*
025700160902     C                   eval      vindta = s_vindta
025800010601     C  N70              update    tivin000
025900010330     C*
026000030822     C  N70              enddo
026100010601     C
026200010601     C*
026300160902     C* Stabilisco l'esito della traduzione
026400160902     C                   select
026500160902     C*
026600160902     C* Se non ci sono record con errori => stato OK
026700160902     C                   when      §ctrno = 0 and
026800160902     C                             §ctrmo = 0
026900010330     C                   eval      wrkesito = '0'
027000160902     C*
027100160902     C* Se nessun record OK => errore totale
027200160908     C                   when      §ctrokvb = 0
027300160902     C                   eval      wrkesito = '2'
027400160902     C*
027500160902     C* Diversamente => errore parziale/warning
027600160902     C                   other
027700160902     C                   eval      wrkesito = '1'
027800160902     C*
027900160902     C                   endsl
028000160908     C*
028100160908     C                   endif
028200160908     C
028300160908     C                   if        chkcall = '1'
028400160908     C                   eval      wrkesito = '2'
028500160908     C                   endif
028600010330     C*
028700010330     C                   if        %open(tivin00r)
028800010330     C                   close     tivin00r
028900010330     C                   endif
029000160908     C*
029100160908     C                   if        %open(fivabwwr)
029200160908     C                   close     fivabwwr
029300160908     C                   endif
029400160908     C                   if        %open(fivatwwr)
029500160908     C                   close     fivatwwr
029600160908     C                   endif
029700160908     C*
029800160908     C                   if        not *in50        and
029900160908     C                             wrkesito <> '2'  and
030000160908     C                             vlrpoi <> *zeros
030100160908     C                   exsr      invio
030200160908     C                   endif
030300010330     C*
030400010330     C                   ENDSR
030500010330     C***
030600160920
030700160920
030800160920     C*--------------------------------------------------------
030900160920     C* ADDINT - AGGIUNTA INTESTAZIONI COLONNE                *
031000160920     C*--------------------------------------------------------
031100160920     C     ADDINT        BEGSR
031200160920     C*
031300160920     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
031400160920     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
031500160920     C                             %trim(intcol(3))+%trim(intcol(4))+
031600160920     C                             %trim(intcol(5))<>*blanks
031700160920     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
031800160920     C                                      %trim(intcol(3))+%trim(intcol(4))+
031900160920     C                                      %trim(intcol(5))
032000160920     C     ';':CharCSV   XLATE     vindta        vindta
032100160920     C*
032200160920     C* Splitto i dati
032300160920     C                   CLEAR                   SkSplitCSV_5
032400160920     C                   EVAL      InStringa = vindta
032500160920     C                   EVAL      InSepara  = CharCSV
032600160920     C                   EXSR      SR_SPLIT
032700160920     C*
032800160920     C* Valorizzo le intestazioni colonna
032900160920     C                   EVAL      wGiro = 1
033000160920     C                   EVAL      SkSplitFLD = SkSplitCSV_5
033100160920     C*
033200160920     C                   ENDIF
033300160920     C*
033400160920     C                   ENDSR
033500160920     C***
033600160908
033700160908
033800160908     C*----------------------------------------------------*
033900160908     C*  SCARICAMENTO BUFFER RECORDS VAB IN "WW"
034000160908     C*----------------------------------------------------*
034100160908     C     WRIVAB_WW     BEGSR
034200160908     C*
034300160908     C* Verifica campi "particolari" VAB
034400160908     C                   exsr      repNSP
034500160908     C*
034600160909     C                   setoff                                       45
034700160909     C                   if        depSPE  = *zeros OR
034800160909     C                             depSPE <> vabNSP
034900160909     C                   seton                                        45
035000160909     C                   eval      depSPE  = vabNSP
035100160909     C                   endif
035200160909     C*
035300160909     C                   if        not *in31
035400160909     C                   exsr      EXEVATE
035500160909     C   45              exsr      EXEVATA
035600160909     C   45              exsr      EXEVATB
035700160909     C   45              exsr      EXEVATI
035800160909     C   45              exsr      EXEVATJ
035900160909     C   45              exsr      EXEVATS
036000160909     C   45              write     fivab000                                     => scarico il VAB
036100160908     C                   endif
036200160908     C*
036300160908     C                   ENDSR
036400160908
036500160908
036600160908     C*----------------------------------------------------*
036700160908     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
036800160908     C*----------------------------------------------------*
036900160908     C     REPNSP        BEGSR
037000160908     C*
037100160909     C* NSP => Se necessario stacco un numeratore da AZNUM
037200160909     C                   if        VABNSP = *zeros
037300160908     C                   clear                   TRUL33DS
037400160908     C                   eval      I33OPE = *zeros
037500160908     C                   eval      I33CNU = 302
037600160908     C                   eval      I33NUM = 1
037700160908     C                   movel     TRUL33DS      KPJBU
037800160908     C                   call      'TRUL33R'
037900160908     C                   parm                    KPJBA
038000160908     C                   movel     KPJBU         TRUL33DS
038100160908     C                   if        O33ERR = *zeros
038200160908     C                   z-add     O33NRF        VABNSP
038300160908     C                   z-add     O33NRF        VATNSP
038400160908     C                   else
038500160908     C                   SETON                                        31
038600160908     C                   EVAL      vinmsg = %trimr(vinmsg)
038700160908     C                             + ' ' + 'VABNSP VATNSP'
038800160908     C                   endif
038900160909     C                   endif
039000160908     C*
039100160908     C                   ENDSR
039200160908
039300160908
039400160908     C*----------------------------------------------------*
039500160908     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
039600160908     C*----------------------------------------------------*
039700160908     C     EXEVATA       BEGSR
039800160908     C*
039900160908     C                   EVAL      VATTRC = 'A'
040000160908     C                   EVAL      VATNOT = wVATNOT_A
040100160908     C                   exsr      wrivat                                       => scarico VAT
040200160908     C*
040300160908     C                   ENDSR
040400160908     C*----------------------------------------------------*
040500160908     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
040600160908     C*----------------------------------------------------*
040700160908     C     EXEVATB       BEGSR
040800160908     C*
040900160908     C                   EVAL      VATTRC = 'B'
041000160908     C                   EVAL      VATNOT = wVATNOT_B
041100160908     C                   exsr      wrivat                                       => scarico VAT
041200160908     C*
041300160908     C                   ENDSR
041400160909     C*----------------------------------------------------*
041500160909     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "I"
041600160909     C*----------------------------------------------------*
041700160909     C     EXEVATI       BEGSR
041800160909     C*
041900160909     C                   IF        %len(%trim(wVATNOT_I)) > 35
042000160909     C                   EVAL      VATTRC = 'I'
042100160909     C                   EVAL      VATNOT = %subst(%trim(wVATNOT_I):1:35)
042200160909     C                   exsr      wrivat                                       => scarico VAT
042300160909     C                   EVAL      VATTRC = 'J'
042400160909     C                   EVAL      VATNOT = %subst(%trim(wVATNOT_I):36)
042500160909     C                   exsr      wrivat                                       => scarico VAT
042600160909     C                   ELSE
042700160909     C                   EVAL      VATTRC = 'I'
042800160909     C                   EVAL      VATNOT = wVATNOT_I
042900160909     C                   exsr      wrivat                                       => scarico VAT
043000160909     C                   ENDIF
043100160909     C*
043200160909     C                   ENDSR
043300160909     C*----------------------------------------------------*
043400160909     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "J"
043500160909     C*----------------------------------------------------*
043600160909     C     EXEVATJ       BEGSR
043700160909     C*
043800160909     C                   IF        %len(%trim(wVATNOT_I)) <= 35
043900160909     C                   EVAL      VATTRC = 'J'
044000160909     C                   EVAL      VATNOT = wVATNOT_J
044100160909     C                   exsr      wrivat                                       => scarico VAT
044200160909     C                   ENDIF
044300160909     C*
044400160909     C                   ENDSR
044500160908     C*----------------------------------------------------*
044600160908     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "S"
044700160908     C*----------------------------------------------------*
044800160908     C     EXEVATS       BEGSR
044900160908     C*
045000160908     C                   EVAL      VATTRC = 'S'
045100160908     C                   EVAL      VATNOT = wVATNOT_S
045200160908     C                   exsr      wrivat                                       => scarico VAT
045300160908     C*
045400160908     C                   ENDSR
045500160908     C*----------------------------------------------------*
045600160908     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
045700160908     C*----------------------------------------------------*
045800160916     C     EXEVATE       BEGSR
045900160916     C*
046000160920     C                   EVAL      VATTRC = 'E'
046100160920     C                   EVAL      VATNOT = wVATNOT_E
046200160920     C                   exsr      wrivat                                       => scarico VAT
046300160908     C*
046400160908     C                   ENDSR
046500160908     C*----------------------------------------------------*
046600160908     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
046700160908     C*----------------------------------------------------*
046800160908     C     WRIVAT        BEGSR
046900160908     C*
047000160908     C* Scrivo solo se valorizzato qualcosa
047100160908     C                   IF        VATNOT <> *blanks
047200160908     C                   EVAL      VATFGS = VABFGS
047300160908     C                   EVAL      VATCCM = VABCCM
047400160908     C                   EVAL      VATLNP = VABLNP
047500160908     C                   EVAL      VATAAS = VABAAS
047600160908     C                   EVAL      VATNRS = VABNRS
047700160908     C                   EVAL      VATNSP = VABNSP
047800160908     C                   WRITE     FIVAT000
047900160908     C                   ADD       1             §CTROKVT
048000160908     C                   ENDIF
048100160908     C*
048200160908     C                   ENDSR
048300160908
048400160908
048500160908     C*----------------------------------------------------*
048600160908     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE ACCESSORIO
048700160908     C*----------------------------------------------------*
048800160908     C     PREVAT        BEGSR
048900160908     C*
049000160908     C* Compongo il nome del membro da dare al FIVATWWR
049100160908     C                   eval      parmbr = vlrhdl
049200160908     C                   movel     'M'           parmbr
049300160908     C                   eval      parccm = %subst(vlrKSC:2:7)
049400160908     C                   eval      paropz = '1'
049500160908     C*
049600160908     C* Effettuo la chiamata al CLLE preposto
049700160908     C                   call(e)   'TITVVTC'
049800160908     C                   parm                    parccm
049900160908     C                   parm                    parmbr
050000160908     C                   parm                    paropz
050100160908     C*
050200160908     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
050300160908     C                   if        %error
050400160908     C                   movel     '1'           chkcall
050500160908     C                   else
050600160908     C                   movel     '0'           chkcall
050700160908     C                   endif
050800160908     C*
050900160908     C                   ENDSR
051000160908
051100160908
051200160908      /TITLE Invio dei dati al punto operativo.
051300160908     C     invio         BEGSR
051400160908     C*
051500160908     C* 1° invio FIVAT
051600160908     C                   reset                   dscmz
051700160908     C                   move      vlrpoi        cmzdst
051800160908     C                   eval      cmzfld = 'FIVATWWR'
051900160908     C                   eval      cmzmbd = vlrhdl
052000160908     C                   eval      %subst(cmzmbd:1:1) = 'M'
052100160908     C***                if        prmfir = *blanks
052200160908     C                   eval      cmzfla = 'FIVAT00F'
052300160908     C                   eval      cmzmba = 'FIVAT00F'
052400160908     C***                else
052500160908     C***                eval      cmzfla = prmfir
052600160908     C***                eval      cmzmba = prmfir
052700160908     C***                endif
052800160908     C                   eval      cmznrr = *zeros
052900160908     C                   move      §ctrokvt      cmznrr
053000160908     C                   eval      cmzlba = vlrfl1
053100160908     C                   call(e)   'TIS711C'
053200160908     C                   parm                    dscmz
053300160908     C                   parm      *blanks       esito
053400160908     C                   if        %error
053500160908     C                             or cmzerr = '1'
053600160908     C                             or esito  = '1'
053700160908     C                   eval      wrkesito = '3'
053800160908     C                   else
053900160908     C*
054000160908     C* 2° invio FIVAB
054100160908     C                   reset                   dscmz
054200160908     C                   move      vlrpoi        cmzdst
054300160908     C                   eval      cmzfld = vlrfou
054400160908     C                   eval      cmzmbd = vlrhdl
054500160908     C                   eval      %subst(cmzmbd:1:1) = 'M'
054600160908     C***                if        prmfir = *blanks
054700160908     C                   eval      cmzfla = 'FIVAB00F'
054800160908     C                   eval      cmzmba = 'FIVAB00F'
054900160908     C***                else
055000160908     C***                eval      cmzfla = prmfir
055100160908     C***                eval      cmzmba = prmfir
055200160908     C***                endif
055300160908     C                   eval      cmznrr = *zeros
055400160908     C                   move      §ctrokvb      cmznrr
055500160908     C                   eval      cmzlba = vlrfl1
055600160908     C                   call(e)   'TIS711C'
055700160908     C                   parm                    dscmz
055800160908     C                   parm      *blanks       esito
055900160908     C                   if        %error
056000160908     C                             or cmzerr = '1'
056100160908     C                             or esito  = '1'
056200160908     C                   eval      wrkesito = '3'
056300160908     C                   endif
056400160908     C                   endif
056500160908     C*
056600160908     C                   ENDSR
056700160908     C***
056800010601
056900010601
057000010330     C*----------------------------------------------------*
057100030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
057200010330     C*----------------------------------------------------*
057300010330     C     INZVAR        BEGSR
057400010330     C*
057500030822     C* Inizializzo variabili di wrk
057600160902     C                   Z-ADD     *zeros        Num5_0            5 0
057700030822     C*
057800030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
057900160908     C                   CLEAR                   FIVAB000
058000160908     C                   CLEAR                   FIVAT000
058100160908     C                   CLEAR                   wVABNOT
058200160902     C                   CLEAR                   wVATNOT_A
058300160902     C                   CLEAR                   wVATNOT_B
058400160902     C                   CLEAR                   wVATNOT_E
058500150420     C                   CLEAR                   wVATNOT_I
058600160908     C                   CLEAR                   wVATNOT_J
058700150420     C                   CLEAR                   wVATNOT_S
058800030822     C                   CLEAR                   SkSplitCSV
058900160902     C                   CLEAR                   wFlgCAS
059000030822     C                   EVAL      i = 1
059100030822     C*
059200030822     C* Reimposto i valori di default
059300030822     C                   EXSR      DEFCAM
059400030822     C*
059500010330     C                   ENDSR
059600160902
059700160902
059800010330     C*----------------------------------------------------*
059900030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
060000010330     C*----------------------------------------------------*
060100010330     C     DEFCAM        BEGSR
060200110706     C*
060300160906     C                   SETOFF                                       7172
060400030822     C*
060500030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
060600030715     C* e delimitatore testo.
060700030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
060800030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
060900030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
061000150420     C                   IF        %subst(vlrppt:5:1) <> *blanks
061100150420     C                   EVAL      pptCSV = %subst(vlrppt:5:1)
061200150420     C                   ELSE
061300150420     C                   EVAL      pptCSV = ';'
061400150420     C                   ENDIF
061500150420     C                   IF        %subst(vlrppt:6:1) <> *blanks
061600150420     C                   EVAL      pptCHR = %subst(vlrppt:6:1)
061700150420     C                   ELSE
061800150420     C                   EVAL      pptCHR = ' '
061900150420     C                   ENDIF
062000090403     C*
062100090403     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
062200160902     C                   EVAL      CharSOS = CharNUM
062300030822     C*
062400160920     C* Reperisco il flag che mi indica se:
062500160920     C* - acquisire dato su AS/400
062600160920     C* - acquisire dato su AS/400 + aggiunta intestazione colonne
062700160920     C* - reinviare al cliente
062800160920     C* - reinviare al cliente + aggiunta intestazione colonne
062900160920     C*
063000160920     C                   IF        %subst(vlrppt:1:1) = 'S' OR
063100160920     C                             %subst(vlrppt:1:1) = '1'
063200160902     C                   SETON                                        50        * OUT => Download
063300160920     C                   ELSE
063400160902     C                   SETOFF                                       50        * OUT => File WW
063500160920     C                   ENDIF
063600160920     C*
063700160920     C                   IF        %subst(vlrppt:1:1) = '1' OR
063800160920     C                             %subst(vlrppt:1:1) = '2'
063900160920     C                   SETON                                        60        * SI aggiunta INTEST
064000160920     C                   ELSE
064100160920     C                   SETOFF                                       60        * NO aggiunta INTEST
064200160920     C                   ENDIF
064300030822     C*
064400030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
064500030822     C                   EVAL      posDaDft = 1
064600030822     C                   EVAL      posADft  = 0
064700030822     C                   EVAL      wGiroDft = 0
064800030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
064900030822     C                             posDaDft > 0
065000030822     C*
065100030822     C* Gestisco il 1° giro
065200030822     C                   IF        wGiroDft = 0
065300030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
065400030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
065500030822     C* Incremento il contatore dei "giri"
065600030822     C                   EVAL      wGiroDft = 1
065700030822     C                   ELSE
065800030822     C                   EVAL      posDaDft = posADft
065900030822     C                   ENDIF
066000030822     C* Eseguo lo scan x trovare la fine del campo corrente
066100030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
066200030822     C*
066300030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
066400030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
066500030822     C                   IF        posDaDft > 0 AND
066600030822     C                             posADft  > 0
066700110706     C* RMN
066800110706     C                   IF        %subst(
066900110706     C                             %subst(vlrppt:posDaDft+1:
067000110706     C                             posADft-posDaDft-1):1:3)
067100110706     C                             = 'RMN'
067200110706     C                   IF        %trim(%subst(
067300110706     C                             %subst(vlrppt:posDaDft+1:
067400110706     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067500110706     C                             = '*NSP'
067600160902     C                   SETON                                        71
067700110706     C                   ENDIF
067800110706     C                   ENDIF
067900060331     C* FGS
068000060331     C                   IF        %subst(
068100060331     C                             %subst(vlrppt:posDaDft+1:
068200060331     C                             posADft-posDaDft-1):1:3)
068300060331     C                             = 'FGS'
068400060331     C                   EVAL      PiStr=%trim(%subst(
068500060331     C                             %subst(vlrppt:posDaDft+1:
068600060331     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068700060331     C                   EXSR      CHKNUM
068800060331     C                   IF        PiInt=*on
068900060331     C                   Z-ADD     PiVal         VABFGS
069000060331     C                   ENDIF
069100060331     C                   ENDIF
069200030822     C* NCL
069300030822     C                   IF        %subst(
069400030822     C                             %subst(vlrppt:posDaDft+1:
069500030822     C                             posADft-posDaDft-1):1:3)
069600030822     C                             = 'NCL'
069700030822     C                   EVAL      PiStr=%trim(%subst(
069800030822     C                             %subst(vlrppt:posDaDft+1:
069900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070000030822     C                   EXSR      CHKNUM
070100030822     C                   IF        PiInt=*on
070200030822     C                   Z-ADD     PiVal         VABNCL
070300030822     C                   ENDIF
070400030822     C                   ENDIF
070500030822     C* CCM
070600030822     C                   IF        %subst(
070700030822     C                             %subst(vlrppt:posDaDft+1:
070800030822     C                             posADft-posDaDft-1):1:3)
070900030822     C                             = 'CCM'
071000030822     C                   EVAL      PiStr=%trim(%subst(
071100030822     C                             %subst(vlrppt:posDaDft+1:
071200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071300030822     C                   EXSR      CHKNUM
071400030822     C                   IF        PiInt=*on
071500030822     C                   Z-ADD     PiVal         VABCCM
071600030822     C                   ENDIF
071700030822     C                   ENDIF
071800030822     C* LNP
071900030822     C                   IF        %subst(
072000030822     C                             %subst(vlrppt:posDaDft+1:
072100030822     C                             posADft-posDaDft-1):1:3)
072200030822     C                             = 'LNP'
072300030822     C                   EVAL      PiStr=%trim(%subst(
072400030822     C                             %subst(vlrppt:posDaDft+1:
072500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072600030822     C                   EXSR      CHKNUM
072700030822     C                   IF        PiInt=*on
072800030822     C                   Z-ADD     PiVal         VABLNP
072900030822     C                   ENDIF
073000030822     C                   ENDIF
073100030822     C* NRS
073200030822     C                   IF        %subst(
073300030822     C                             %subst(vlrppt:posDaDft+1:
073400030822     C                             posADft-posDaDft-1):1:3)
073500030822     C                             = 'NRS'
073600030822     C                   EVAL      PiStr=%trim(%subst(
073700030822     C                             %subst(vlrppt:posDaDft+1:
073800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073900030822     C                   EXSR      CHKNUM
074000030822     C                   IF        PiInt=*on
074100030822     C                   Z-ADD     PiVal         VABNRS
074200030822     C                   ENDIF
074300030822     C                   ENDIF
074400160906     C* CTR
074500030822     C                   IF        %subst(
074600030822     C                             %subst(vlrppt:posDaDft+1:
074700030822     C                             posADft-posDaDft-1):1:3)
074800030822     C                             = 'CTR'
074900030822     C                   EVAL      PiStr=%trim(%subst(
075000030822     C                             %subst(vlrppt:posDaDft+1:
075100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075200030822     C                   EXSR      CHKNUM
075300030822     C                   IF        PiInt=*on
075400160906     C                   Z-ADD     PiVal         VABCTR
075500030822     C                   ENDIF
075600030822     C                   ENDIF
075700160906     C* CTE (ovvero CTR per Estero)
075800160906     C                   IF        %subst(
075900160906     C                             %subst(vlrppt:posDaDft+1:
076000160906     C                             posADft-posDaDft-1):1:3)
076100160906     C                             = 'CTE'
076200160906     C                   EVAL      PiStr=%trim(%subst(
076300160906     C                             %subst(vlrppt:posDaDft+1:
076400160906     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076500160906     C                   EXSR      CHKNUM
076600160906     C                   IF        PiInt=*on
076700160906     C                   Z-ADD     PiVal         wVABCTR_E
076800160906     C                   SETON                                        72
076900160906     C                   ENDIF
077000160906     C                   ENDIF
077100030822     C* PKB
077200030822     C                   IF        %subst(
077300030822     C                             %subst(vlrppt:posDaDft+1:
077400030822     C                             posADft-posDaDft-1):1:3)
077500030822     C                             = 'PKB'
077600030822     C                   EVAL      PiStr=%trim(%subst(
077700030822     C                             %subst(vlrppt:posDaDft+1:
077800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077900030822     C                   EXSR      CHKNUM
078000030822     C                   IF        PiNum=*on
078100030822     C                   Z-ADD     PiVal         VABPKB
078200030822     C                   ENDIF
078300030822     C                   ENDIF
078400030822     C* VLB
078500030822     C                   IF        %subst(
078600030822     C                             %subst(vlrppt:posDaDft+1:
078700030822     C                             posADft-posDaDft-1):1:3)
078800030822     C                             = 'VLB'
078900030822     C                   EVAL      PiStr=%trim(%subst(
079000030822     C                             %subst(vlrppt:posDaDft+1:
079100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079200030822     C                   EXSR      CHKNUM
079300030822     C                   IF        PiNum=*on
079400030822     C                   Z-ADD     PiVal         VABVLB
079500030822     C                   ENDIF
079600030822     C                   ENDIF
079700030822     C* QFT
079800030822     C                   IF        %subst(
079900030822     C                             %subst(vlrppt:posDaDft+1:
080000030822     C                             posADft-posDaDft-1):1:3)
080100030822     C                             = 'QFT'
080200030822     C                   EVAL      PiStr=%trim(%subst(
080300030822     C                             %subst(vlrppt:posDaDft+1:
080400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080500030822     C                   EXSR      CHKNUM
080600030822     C                   IF        PiNum=*on
080700030822     C                   Z-ADD     PiVal         VABQFT
080800030822     C                   ENDIF
080900030822     C                   ENDIF
081000030822     C* CBO
081100030822     C                   IF        %subst(
081200030822     C                             %subst(vlrppt:posDaDft+1:
081300030822     C                             posADft-posDaDft-1):1:3)
081400030822     C                             = 'CBO'
081500030822     C                   EVAL      VABCBO=%trim(%subst(
081600030822     C                             %subst(vlrppt:posDaDft+1:
081700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081800030822     C                   ENDIF
081900030822     C* TSP
082000030822     C                   IF        %subst(
082100030822     C                             %subst(vlrppt:posDaDft+1:
082200030822     C                             posADft-posDaDft-1):1:3)
082300030822     C                             = 'TSP'
082400030822     C                   EVAL      VABTSP=%trim(%subst(
082500030822     C                             %subst(vlrppt:posDaDft+1:
082600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082700030822     C                   ENDIF
082800030822     C* VAS
082900030822     C                   IF        %subst(
083000030822     C                             %subst(vlrppt:posDaDft+1:
083100030822     C                             posADft-posDaDft-1):1:3)
083200030822     C                             = 'VAS'
083300030822     C                   EVAL      VABVAS=%trim(%subst(
083400030822     C                             %subst(vlrppt:posDaDft+1:
083500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
083600030822     C                   ENDIF
083700030822     C* VCA
083800030822     C                   IF        %subst(
083900030822     C                             %subst(vlrppt:posDaDft+1:
084000030822     C                             posADft-posDaDft-1):1:3)
084100030822     C                             = 'VCA'
084200030822     C                   EVAL      VABVCA=%trim(%subst(
084300030822     C                             %subst(vlrppt:posDaDft+1:
084400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
084500030822     C                   ENDIF
084600030822     C* GCA
084700030822     C                   IF        %subst(
084800030822     C                             %subst(vlrppt:posDaDft+1:
084900030822     C                             posADft-posDaDft-1):1:3)
085000030822     C                             = 'GCA'
085100030822     C                   EVAL      VABGCA=%trim(%subst(
085200030822     C                             %subst(vlrppt:posDaDft+1:
085300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
085400030822     C                   ENDIF
085500030822     C* CTM
085600030822     C                   IF        %subst(
085700030822     C                             %subst(vlrppt:posDaDft+1:
085800030822     C                             posADft-posDaDft-1):1:3)
085900030822     C                             = 'CTM'
086000030822     C                   EVAL      VABCTM=%trim(%subst(
086100030822     C                             %subst(vlrppt:posDaDft+1:
086200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
086300030822     C                   ENDIF
086400030822     C* FFD
086500030822     C                   IF        %subst(
086600030822     C                             %subst(vlrppt:posDaDft+1:
086700030822     C                             posADft-posDaDft-1):1:3)
086800030822     C                             = 'FFD'
086900030822     C                   EVAL      VABFFD=%trim(%subst(
087000030822     C                             %subst(vlrppt:posDaDft+1:
087100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
087200030822     C                   ENDIF
087300030822     C* VAD
087400030822     C                   IF        %subst(
087500030822     C                             %subst(vlrppt:posDaDft+1:
087600030822     C                             posADft-posDaDft-1):1:3)
087700030822     C                             = 'VAD'
087800030822     C                   EVAL      VABVAD=%trim(%subst(
087900030822     C                             %subst(vlrppt:posDaDft+1:
088000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
088100030822     C                   ENDIF
088200030822     C* GMA
088300030822     C                   IF        %subst(
088400030822     C                             %subst(vlrppt:posDaDft+1:
088500030822     C                             posADft-posDaDft-1):1:3)
088600030822     C                             = 'GMA'
088700030822     C                   EVAL      VABGMA=%trim(%subst(
088800030822     C                             %subst(vlrppt:posDaDft+1:
088900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
089000030822     C                   ENDIF
089100030822     C* GGA
089200030822     C                   IF        %subst(
089300030822     C                             %subst(vlrppt:posDaDft+1:
089400030822     C                             posADft-posDaDft-1):1:3)
089500030822     C                             = 'GGA'
089600030822     C                   EVAL      VABGGA=%trim(%subst(
089700030822     C                             %subst(vlrppt:posDaDft+1:
089800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
089900030822     C                   ENDIF
090000030822     C* GVA
090100030822     C                   IF        %subst(
090200030822     C                             %subst(vlrppt:posDaDft+1:
090300030822     C                             posADft-posDaDft-1):1:3)
090400030822     C                             = 'GVA'
090500030822     C                   EVAL      VABGVA=%trim(%subst(
090600030822     C                             %subst(vlrppt:posDaDft+1:
090700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
090800030822     C                   ENDIF
090900030822     C* TC1
091000030822     C                   IF        %subst(
091100030822     C                             %subst(vlrppt:posDaDft+1:
091200030822     C                             posADft-posDaDft-1):1:3)
091300030822     C                             = 'TC1'
091400030822     C                   EVAL      VABTC1=%trim(%subst(
091500030822     C                             %subst(vlrppt:posDaDft+1:
091600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
091700030822     C                   ENDIF
091800030822     C* TC2
091900030822     C                   IF        %subst(
092000030822     C                             %subst(vlrppt:posDaDft+1:
092100030822     C                             posADft-posDaDft-1):1:3)
092200030822     C                             = 'TC2'
092300030822     C                   EVAL      VABTC2=%trim(%subst(
092400030822     C                             %subst(vlrppt:posDaDft+1:
092500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
092600030822     C                   ENDIF
092700030822     C* ...
092800030822     C                   ENDIF
092900030822     C                   ENDDO
093000020204     C*
093100010330     C                   ENDSR
093200010607     C*----------------------------------------------------*
093300021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
093400010607     C*----------------------------------------------------*
093500010607     C     IMPVAB        BEGSR
093600010607     C*
093700010607     C                   SETOFF                                       3132
093800030515     C*
093900010607     C                   MOVEL     datcor        VABAAS
094000010607     C                   MOVE      datcor        VABMGS
094100160902     C*
094200160902     C* Dopo aver splittato nei vari campi eseguo 1 prima "normalizzazione" x eliminare
094300160902     C* i caratteri di delimitazione testo e gli apici
094400160902     C                   EVAL      SkSplitCSV = SkSplitCSV_5
094500160902     C                   EVAL      i = 1
094600160902     C                   DOW       i < %elem(SkSplitCSV)
094700160902     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
094800160902     C     '''':' '      XLATE     SkSplitCSV(i) SkSplitCSV(i)
094900160902     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
095000160902     C                   EVAL      i = i + 1
095100160902     C                   ENDDO
095200010607     C*
095300030715     C**********
095400160902     C* Valorizzo la lista delle INTESTAZIONI colonne cliente
095500030715     C**********
095600030715     C                   IF        wGiro = *zeros
095700160902     C*
095800160902     C                   EVAL      wGiro = 1
095900160902     C                   EVAL      SkSplitFLD = SkSplitCSV
096000160902     C*
096100160902     C                   ELSE
096200160902     C*
096300160902     C**********
096400160902     C* Elaboro le righe di DATI cliente
096500160902     C**********
096600160902     C                   EVAL      wGiro = 2
096700030715     C*
096800030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
096900030715     C                   EVAL      i = 1
097000030715     C                   DOW       i < %elem(SkSplitFLD)
097100030715     C*
097200030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
097300030715     C                   IF        SkSplitFLD(i) <> *blanks AND
097400030715     C                             SkSplitCSV(i) <> *blanks
097500130624     C*** ATB
097600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
097700150420     C                   EVAL      VABATB = SkSplitCSV(i)
097800030721     C                   ENDIF
097900030715     C*** RSD
098000160916     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
098100030715     C                   EVAL      VABRSD = SkSplitCSV(i)
098200030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
098300030715     C     '@':'A'       XLATE     VABRSD        VABRSD
098400030715     C                   ENDIF
098500030721     C*** RD2
098600160916     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
098700160902     C                   EVAL      VABRD2 = SkSplitCSV(i)
098800160902     C                   ENDIF
098900030715     C*** IND
099000160916     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
099100030715     C                   EVAL      VABIND = SkSplitCSV(i)
099200030715     C                   ENDIF
099300030715     C*** LOD
099400160916     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
099500160909     C                   EVAL      VABLOD = SkSplitCSV(i)
099600030715     C                   ENDIF
099700030715     C*** PRD
099800160916     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
099900030715     C                   EVAL      VABPRD = SkSplitCSV(i)
100000030715     C                   ENDIF
100100160902     C*** CAD
100200160916     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
100300160902     C                   EVAL      VABCAD = SkSplitCSV(i)
100400160902     C                   ENDIF
100500030721     C*** NZD
100600160916     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
100700030721     C                   EVAL      VABNZD = SkSplitCSV(i)
100800030721     C                   ENDIF
100900160916     C*** RMA
101000160916     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
101100160916     C                   EVAL      VABRMA = SkSplitCSV(i)
101200150515     C                   ENDIF
101300030715     C*** NOT
101400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
101500030715     C                   EVAL      VABNOT = SkSplitCSV(i)
101600030715     C                   ENDIF
101700030715     C*** NT2
101800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
101900030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
102000030715     C                   ENDIF
102100030715     C*** VCA
102200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
102300030715     C                   EVAL      VABVCA = SkSplitCSV(i)
102400030715     C                   ENDIF
102500030715     C*** VAS
102600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
102700030715     C                   EVAL      VABVAS = SkSplitCSV(i)
102800030715     C                   ENDIF
102900030715     C*** FFD
103000160902     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
103100030715     C                   EVAL      VABFFD = SkSplitCSV(i)
103200030715     C                   ENDIF
103300160902     C*** CBO
103400160902     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
103500160902     C                   EVAL      VABCBO = SkSplitCSV(i)
103600150420     C                   ENDIF
103700030721     C*** GC1
103800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
103900150420     C                   IF        %len(%trim(SkSplitCSV(i))) = 1
104000150420     C                   MOVEL     *blanks       wGC               1
104100150420     C                   EVAL      wGC = %trim(SkSplitCSV(i))
104200150420     C                   IF        SkSplitCSV(i) = 'M' OR
104300150420     C                             SkSplitCSV(i) = 'P'
104400150420     C                   MOVE(P)   wGC           VABGC1
104500150420     C                   ELSE
104600150420     C                   MOVEL(P)  wGC           VABGC1
104700150420     C                   ENDIF
104800150420     C                   ENDIF
104900030721     C                   ENDIF
105000030721     C*** GC2
105100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
105200120314     C                   IF        %len(%trim(SkSplitCSV(i))) = 1
105300120314     C                   MOVEL     *blanks       wGC               1
105400120314     C                   EVAL      wGC = %trim(SkSplitCSV(i))
105500120314     C                   IF        SkSplitCSV(i) = 'M' OR
105600120314     C                             SkSplitCSV(i) = 'P'
105700120314     C                   MOVE(P)   wGC           VABGC2
105800120314     C                   ELSE
105900120314     C                   MOVEL(P)  wGC           VABGC2
106000120314     C                   ENDIF
106100120314     C                   ENDIF
106200030721     C                   ENDIF
106300030721     C*** TSP
106400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
106500030721     C                   EVAL      VABTSP = SkSplitCSV(i)
106600030721     C                   ENDIF
106700030721     C*** NAS
106800160902     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
106900030721     C                   EVAL      VABNAS = SkSplitCSV(i)
107000030721     C                   ENDIF
107100030721     C*** TIC
107200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
107300030721     C                   EVAL      VABTIC = SkSplitCSV(i)
107400030721     C                   ENDIF
107500030721     C*** GCA
107600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
107700030721     C                   EVAL      VABGCA = SkSplitCSV(i)
107800030721     C                   ENDIF
107900030721     C*** XCO
108000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
108100030721     C                   EVAL      VABXCO = SkSplitCSV(i)
108200030721     C                   ENDIF
108300030721     C*** CTM
108400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
108500030721     C                   EVAL      VABCTM = SkSplitCSV(i)
108600030721     C                   ENDIF
108700030721     C*** TCR
108800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
108900030721     C                   EVAL      VABTCR = SkSplitCSV(i)
109000030721     C                   ENDIF
109100030721     C*** CTS
109200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
109300030721     C                   EVAL      VABCTS = SkSplitCSV(i)
109400030721     C                   ENDIF
109500030721     C*** FTM
109600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
109700030721     C                   EVAL      VABFTM = SkSplitCSV(i)
109800030721     C                   ENDIF
109900030721     C*** VAD
110000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
110100030721     C                   EVAL      VABVAD = SkSplitCSV(i)
110200030721     C                   ENDIF
110300030721     C*** GMA
110400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
110500030721     C                   EVAL      VABGMA = SkSplitCSV(i)
110600030721     C                   ENDIF
110700030721     C*** GGA
110800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
110900030721     C                   EVAL      VABGGA = SkSplitCSV(i)
111000030721     C                   ENDIF
111100030721     C*** GVA
111200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
111300030721     C                   EVAL      VABGVA = SkSplitCSV(i)
111400030721     C                   ENDIF
111500030721     C*** TC1
111600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
111700030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
111800030721     C                   ENDIF
111900030721     C*** TC2
112000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
112100030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
112200030721     C                   ENDIF
112300030721     C*** SCL
112400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
112500030721     C                   EVAL      VABSCL = SkSplitCSV(i)
112600030721     C                   ENDIF
112700030721     C*** RMO
112800160902     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
112900030721     C                   EVAL      VABRMO = SkSplitCSV(i)
113000030721     C                   ENDIF
113100160902     C*** CMO
113200160902     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
113300160902     C                   EVAL      VABCMO = SkSplitCSV(i)
113400160902     C                   ENDIF
113500030721     C*** NMO
113600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
113700030721     C                   EVAL      VABNMO = SkSplitCSV(i)
113800030721     C                   ENDIF
113900160908     C*** NOT_A
114000160916     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
114100160908     C                   EVAL      wVATNOT_A = SkSplitCSV(i)
114200160908     C                   ENDIF
114300160902     C*** NOT_B
114400160916     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
114500160902     C                   EVAL      wVATNOT_B = SkSplitCSV(i)
114600160902     C                   ENDIF
114700150420     C*** NOT_I
114800160916     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
114900150420     C                   EVAL      wVATNOT_I = SkSplitCSV(i)
115000150420     C                   ENDIF
115100150420     C*** NOT_S
115200160916     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_S'
115300150420     C                   EVAL      wVATNOT_S = SkSplitCSV(i)
115400150420     C                   ENDIF
115500160909     C*** NOT_E
115600160916     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
115700160909     C                   EVAL      wVATNOT_E = SkSplitCSV(i)
115800160909     C                   ENDIF
115900030721     C*
116000070705     C* Reperisco quindi i campi numerici...
116100030721     C*** RMN
116200160916     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
116300030721     C                   EVAL      PiStr=SkSplitCSV(i)
116400030721     C                   EXSR      CHKNUM
116500030721     C                   IF        PiInt=*on
116600030721     C                   Z-ADD     PiVal         VABRMN
116700160909     C                   Z-ADD     PiVal         VABNSP
116800030721     C                   ELSE
116900030721     C                   SETON                                        32
117000030721     C                   EVAL      VABRMN = 1
117100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117200160909     C                             + ' ' + 'VABRMN VABNSP'
117300030721     C                   ENDIF
117400030721     C                   ENDIF
117500030721     C*** NSP
117600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
117700030721     C                   EVAL      PiStr=SkSplitCSV(i)
117800030721     C                   EXSR      CHKNUM
117900100517     C                   IF        PiInt=*on
118000030721     C                   Z-ADD     PiVal         VABNSP
118100030721     C                   ELSE
118200030721     C                   SETON                                        32
118300030721     C                   EVAL      VABNSP = *zeros
118400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118500030721     C                             + ' ' + 'VABNSP'
118600030721     C                   ENDIF
118700030721     C                   ENDIF
118800160902     C*** NCL
118900160916     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
119000030721     C                   EVAL      PiStr=SkSplitCSV(i)
119100030721     C                   EXSR      CHKNUM
119200030721     C                   IF        PiInt=*on
119300030721     C                   Z-ADD     PiVal         VABNCL
119400030721     C                   ELSE
119500030721     C                   SETON                                        32
119600030721     C                   EVAL      VABNCL = *zeros
119700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119800030721     C                             + ' ' + 'VABNCL'
119900030721     C                   ENDIF
120000030721     C                   ENDIF
120100030721     C*** PKB
120200160916     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
120300030721     C                   EVAL      PiStr=SkSplitCSV(i)
120400030721     C                   EXSR      CHKNUM
120500030721     C                   IF        PiNum=*on
120600160920     C                   EVAL      PiVal = PiVal / 10                           * PERS
120700030721     C                   Z-ADD     PiVal         VABPKB
120800030721     C                   ELSE
120900030721     C                   SETON                                        32
121000030721     C                   EVAL      VABPKB = *zeros
121100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121200030721     C                             + ' ' + 'VABPKB'
121300030721     C                   ENDIF
121400030721     C                   ENDIF
121500160908     C*** CAS
121600160916     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
121700160915     C                   IF        SkSplitCSV(i) <> *blanks AND
121800160915     C                             SkSplitCSV(i) <> *zeros  AND
121900160920     C                             SkSplitCSV(i) <> '0,000' AND
122000160921     C                             SkSplitCSV(i) <> '0,0' AND
122100160920     C                             SkSplitCSV(i) <> '0'
122200160908     C                   EVAL      wFlgCAS = 1
122300160908     C                   ENDIF
122400160902     C                   EVAL      PiStr=SkSplitCSV(i)
122500160902     C                   EXSR      CHKNUM
122600160902     C                   IF        PiNum=*on
122700160921     C***                EVAL      PiVal = PiVal / 100                          * PERS
122800160902     C                   Z-ADD     PiVal         VABCAS
122900160902     C                   MOVEL     'EUR'         VABVCA
123000160902     C                   ELSE
123100160902     C                   SETON                                        32
123200160902     C                   EVAL      VABCAS = *zeros
123300160902     C                   EVAL      vinmsg = %trimr(vinmsg)
123400160902     C                             + ' ' + 'VABCAS'
123500160902     C                   ENDIF
123600160902     C                   ENDIF
123700030721     C*** CCM
123800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
123900030721     C                   EVAL      PiStr=SkSplitCSV(i)
124000030721     C                   EXSR      CHKNUM
124100030721     C                   IF        PiInt=*on      AND
124200030721     C                             PiVal<=9999999 AND
124300030721     C                             PiVal>*zeros
124400030721     C                   Z-ADD     PiVal         VABCCM
124500030721     C                   ELSE
124600030721     C                   SETON                                        32
124700030721     C                   EVAL      VABCCM = *zeros
124800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124900030721     C                             + ' ' + 'VABCCM'
125000030721     C                   ENDIF
125100030721     C                   ENDIF
125200030721     C*** LNP
125300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
125400030721     C                   EVAL      PiStr=SkSplitCSV(i)
125500030721     C                   EXSR      CHKNUM
125600030721     C                   IF        PiInt=*on    AND
125700030721     C                             PiVal<=999   AND
125800030721     C                             PiVal>*zeros
125900030721     C                   Z-ADD     PiVal         VABLNP
126000030721     C                   ELSE
126100030721     C                   SETON                                        32
126200030721     C                   EVAL      VABLNP = *zeros
126300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126400030721     C                             + ' ' + 'VABLNP'
126500030721     C                   ENDIF
126600030721     C                   ENDIF
126700030721     C*** NRS
126800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
126900030721     C                   EVAL      PiStr=SkSplitCSV(i)
127000030721     C                   EXSR      CHKNUM
127100030721     C                   IF        PiInt=*on AND
127200030721     C                             PiVal<=99
127300030721     C                   Z-ADD     PiVal         VABNRS
127400030721     C                   ELSE
127500030721     C                   SETON                                        32
127600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127700030721     C                             + ' ' + 'VABNRS'
127800030721     C                   ENDIF
127900030721     C                   ENDIF
128000030721     C*** LNA
128100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
128200120314     C                   IF        %trim(SkSplitCSV(i)) <> *zeros  AND
128300120314     C                             %trim(SkSplitCSV(i)) <> '0'     AND
128400120314     C                             %trim(SkSplitCSV(i)) <> '000'   AND
128500120314     C                             %trim(SkSplitCSV(i)) <> *blanks
128600030721     C                   EVAL      PiStr=SkSplitCSV(i)
128700030721     C                   EXSR      CHKNUM
128800030721     C                   IF        PiInt=*on    AND
128900030721     C                             PiVal<=999   AND
129000030721     C                             PiVal>*zeros
129100030721     C                   Z-ADD     PiVal         VABLNA
129200030721     C                   ELSE
129300030721     C                   SETON                                        32
129400030721     C                   EVAL      VABLNA = *zeros
129500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129600030721     C                             + ' ' + 'VABLNA'
129700120314     C                   ENDIF
129800030721     C                   ENDIF
129900030721     C                   ENDIF
130000030721     C*** CTR
130100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
130200030721     C                   EVAL      PiStr=SkSplitCSV(i)
130300030721     C                   EXSR      CHKNUM
130400030721     C                   IF        PiInt=*on  AND
130500030721     C                             PiVal<=999
130600030721     C                   Z-ADD     PiVal         VABCTR
130700030721     C                   ELSE
130800030721     C                   SETON                                        32
130900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131000030721     C                             + ' ' + 'VABCTR'
131100030721     C                   ENDIF
131200030721     C                   ENDIF
131300030721     C*** IAS
131400160902     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
131500030721     C                   EVAL      PiStr=SkSplitCSV(i)
131600030721     C                   EXSR      CHKNUM
131700030721     C                   IF        PiNum=*on
131800030721     C                   Z-ADD     PiVal         VABIAS
131900030721     C                   ELSE
132000030721     C                   SETON                                        32
132100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132200030721     C                             + ' ' + 'VABIAS'
132300030721     C                   ENDIF
132400030721     C                   ENDIF
132500030721     C*** VLB
132600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
132700030721     C                   EVAL      PiStr=SkSplitCSV(i)
132800030721     C                   EXSR      CHKNUM
132900030721     C                   IF        PiNum=*on
133000030721     C                   Z-ADD     PiVal         VABVLB
133100030721     C                   ELSE
133200030721     C                   SETON                                        32
133300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133400030721     C                             + ' ' + 'VABVLB'
133500030721     C                   ENDIF
133600030721     C                   ENDIF
133700030721     C*** QFT
133800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
133900030721     C                   EVAL      PiStr=SkSplitCSV(i)
134000030721     C                   EXSR      CHKNUM
134100030721     C                   IF        PiNum=*on
134200030721     C                   Z-ADD     PiVal         VABQFT
134300030721     C                   ELSE
134400030721     C                   SETON                                        32
134500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134600030721     C                             + ' ' + 'VABQFT'
134700030721     C                   ENDIF
134800030721     C                   ENDIF
134900030721     C*** NCD
135000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
135100030721     C                   EVAL      PiStr=SkSplitCSV(i)
135200030721     C                   EXSR      CHKNUM
135300030721     C                   IF        PiInt=*on      AND
135400030721     C                             PiVal<=9999999
135500030721     C                   Z-ADD     PiVal         VABNCD
135600030721     C                   ELSE
135700030721     C                   SETON                                        32
135800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135900030721     C                             + ' ' + 'VABNCD'
136000030721     C                   ENDIF
136100030721     C                   ENDIF
136200030721     C*** NCA
136300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
136400030721     C                   EVAL      PiStr=SkSplitCSV(i)
136500030721     C                   EXSR      CHKNUM
136600030721     C                   IF        PiInt=*on      AND
136700030721     C                             PiVal<=9999999
136800030721     C                   Z-ADD     PiVal         VABNCA
136900030721     C                   ELSE
137000030721     C                   SETON                                        32
137100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137200030721     C                             + ' ' + 'VABNCA'
137300030721     C                   ENDIF
137400030721     C                   ENDIF
137500030721     C*** ZNC
137600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
137700030721     C                   EVAL      PiStr=SkSplitCSV(i)
137800030721     C                   EXSR      CHKNUM
137900030721     C                   IF        PiInt=*on AND
138000030721     C                             PiVal<=99
138100030721     C                   Z-ADD     PiVal         VABZNC
138200030721     C                   ELSE
138300030721     C                   SETON                                        32
138400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138500030721     C                             + ' ' + 'VABZNC'
138600030721     C                   ENDIF
138700030721     C                   ENDIF
138800030721     C*** DCR
138900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
139000030721     C                   EVAL      PiStr=SkSplitCSV(i)
139100030721     C                   EXSR      CHKNUM
139200030721     C                   IF        PiInt=*on       AND
139300030721     C                             PiVal<=99999999
139400030721     C                   Z-ADD     PiVal         VABDCR
139500030721     C                   ELSE
139600030721     C                   SETON                                        32
139700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
139800030721     C                             + ' ' + 'VABDCR'
139900030721     C                   ENDIF
140000030721     C                   ENDIF
140100030721     C*** HCR
140200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
140300030721     C                   EVAL      PiStr=SkSplitCSV(i)
140400030721     C                   EXSR      CHKNUM
140500030721     C                   IF        PiInt=*on   AND
140600030721     C                             PiVal<=9999
140700030721     C                   Z-ADD     PiVal         VABHCR
140800030721     C                   ELSE
140900030721     C                   SETON                                        32
141000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
141100030721     C                             + ' ' + 'VABHCR'
141200030721     C                   ENDIF
141300030721     C                   ENDIF
141400030721     C*** VMD
141500160902     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
141600030721     C                   EVAL      PiStr=SkSplitCSV(i)
141700030721     C                   EXSR      CHKNUM
141800030721     C                   IF        PiNum=*on
141900030721     C                   Z-ADD     PiVal         VABVMD
142000030721     C                   ELSE
142100030721     C                   SETON                                        32
142200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
142300030721     C                             + ' ' + 'VABVMD'
142400030721     C                   ENDIF
142500030721     C                   ENDIF
142600030721     C*** ANT
142700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
142800030721     C                   EVAL      PiStr=SkSplitCSV(i)
142900030721     C                   EXSR      CHKNUM
143000030721     C                   IF        PiInt=*on        AND
143100030721     C                             PiVal<=999999999
143200030721     C                   Z-ADD     PiVal         VABANT
143300030721     C                   ELSE
143400030721     C                   SETON                                        32
143500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
143600030721     C                             + ' ' + 'VABANT'
143700030721     C                   ENDIF
143800030721     C                   ENDIF
143900160920     C*
144000160920     C********************************************************
144100160920     C* Forzature "dentro ciclo"
144200160920     C********************************************************
144300160920     C*
144400160920     C*
144500160920     C********************************************************
144600030715     C*
144700030715     C                   ENDIF
144800030715     C                   EVAL      i = i + 1
144900030715     C                   ENDDO
145000070705     C*
145100160920     C********************************************************
145200160902     C* Considerazioni particolari "fuori ciclo"
145300160920     C********************************************************
145400160920     C*
145500160920     C*** NOT_A
145600160920     C                   EVAL      wVATNOT_A = VABRSD
145700160920     C*
145800160920     C********************************************************
145900110706     C*
146000160908     C* Verifica campi "particolari" VAB
146100160908     C                   EXSR      CHKVAB
146200160908     C*
146300030715     C                   ENDIF
146400020204     C*
146500010607     C                   ENDSR
146600010607     C*----------------------------------------------------*
146700160908
146800160908
146900160908     C*----------------------------------------------------*
147000160908     C*  CONSIDERAZIONI SU CAMPI PARTICOLARI VAB
147100160908     C*----------------------------------------------------*
147200160908     C     CHKVAB        BEGSR
147300160908     C*
147400160908     C*** RMN
147500160908     C                   IF        *in71
147600160908     C                   EVAL      VABRMN = VABNSP
147700160908     C                   ENDIF
147800160908     C                   IF        VABRMN = *zeros
147900160908     C                   EVAL      VABRMN = 1
148000160908     C                   ENDIF
148100160908     C*** NZD
148200160908     C                   IF        VABNZD = 'IT'  OR
148300160908     C                             VABNZD = 'ITA' OR
148400160908     C                             VABNZD = 'Ita' OR
148500160908     C                             VABNZD = 'ita'
148600160908     C                   EVAL      VABNZD = *blanks
148700160908     C                   ENDIF
148800160908     C*** CTR
148900160908     C                   IF        *in72
149000160908     C                   IF        VABNZD <> *blanks
149100160908     C                   EVAL      VABCTR =  wVABCTR_E
149200160908     C                   ENDIF
149300160908     C                   ENDIF
149400160908     C*** CBO
149500160908     C                   IF        VABCBO = *blanks
149600160908     C                   EVAL      VABCBO = '1'
149700160908     C                   ENDIF
149800160908     C*** NCL
149900160908     C                   IF        VABNCL = *zeros
150000160908     C                   EVAL      VABNCL = 1
150100160908     C                   ENDIF
150200160908     C*** CCM
150300160908     C                   IF        VABCCM = *zeros
150400160908     C                   EVAL      PiStr=VLRKSC
150500160908     C                   EXSR      CHKNUM
150600160908     C                   IF        PiInt=*on
150700160908     C                   Z-ADD     PiVal         VABCCM
150800160908     C                   ELSE
150900160908     C                   SETON                                        31
151000160908     C                   ENDIF
151100160908     C                   ENDIF
151200160908     C*** LNP
151300160908     C                   IF        VABLNP = *zeros
151400160908     C                   IF        VLRPOI = 046 OR
151500160908     C                             VLRPOI = 789 OR
151600160908     C                             VLRPOI = 999
151700160908     C                   MOVEL     VABCCM        VABLNP
151800160908     C                   ELSE
151900160908     C                   EVAL      VABLNP = VLRPOI
152000160908     C                   ENDIF
152100160908     C                   ENDIF
152200160908     C*** LNP
152300160908     C                   IF        VABFGS = *zeros
152400160908     C                   EVAL      VABFGS = VABLNP
152500160908     C                   ENDIF
152600160908     C*** FGS / LNP
152700160908     C                   IF        VABFGS = *zeros OR
152800160908     C                             VABLNP = *zeros
152900160908     C                   SETON                                        31
153000160908     C                   ENDIF
153100160908     C*** CAS
153200160908     C                   IF        wFlgCAS = *zeros
153300160908     C                   EVAL      VABCAS = *zeros
153400160908     C                   EVAL      VABVCA = *blanks
153500160908     C                   ENDIF
153600160908     C*** NOT
153700160908     C                   IF        VABNOT = *blanks AND wVABNOT <> *blanks
153800160908     C                   EVAL      VABNOT = wVABNOT
153900160908     C                   ENDIF
154000160908     C*
154100160908     C* Se provincia nn valorizzata la reperisco
154200160908     C* tramite TISI95R a seconda dei dati d instradamento presenti
154300160908     C                   IF        VABPRD = *blanks
154400160908     C                   CLEAR                   TISI95DS
154500160908     C                   EVAL      I95TCN = '3'
154600160908     C                   Z-ADD     datcor        I95DAT
154700160908     C                   EVAL      I95NAR = VABNZD
154800160908     C                   EVAL      I95CAP = VABCAD
154900160908     C                   EVAL      I95LOC = VABLOD
155000160908     C                   CALL      'TISI95R'
155100160908     C                   PARM                    TISI95DS
155200160908     C                   EVAL      VABPRD = O95PRV
155300160908     C                   ENDIF
155400160908     C*
155500160908     C* Eseguo routine finale x considerazioni specifiche su importi/divise
155600160908     C                   EXSR      CHKIMPDIV
155700160908     C*
155800160908     C                   ENDSR
155900160908     C***
156000010601
156100020204
156200020204     C*----------------------------------------------------*
156300020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
156400020204     C*----------------------------------------------------*
156500020204     C     CHKIMPDIV     BEGSR
156600060524     C*
156700060524     C* Verifico il codice bolla in base al tipo porto e alla presenza o meno del contrassegno
156800160906     C                   IF        vabCAS  > *zeros OR
156900160906     C                             wFlgCAS > *zeros
157000060524     C                   IF        vabCBO = '1'
157100060524     C                   EVAL      vabCBO = '4'
157200060524     C                   ENDIF
157300060524     C                   IF        vabCBO = '2'
157400060524     C                   EVAL      vabCBO = '6'
157500060524     C                   ENDIF
157600060524     C                   ENDIF
157700020204     C*
157800020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
157900020204     C                   Z-ADD     *zeros        wrkDec            9 9
158000020204     C*
158100020204     C* Come prima cosa effettuo considerazioni sulla divisa
158200020204     C                   IF        vabIAS > *zeros
158300150420     C                   IF        vabVAS =  *blanks
158400150420     C                   EVAL      vabVAS =  'EUR'
158500020204     C                   ENDIF
158600020204     C                   ENDIF
158700020204     C*
158800020204     C                   IF        vabCAS > *zeros
158900150420     C                   IF        vabVCA <> *blanks
159000150420     C                   EVAL      vabVCA =  'EUR'
159100020204     C                   ENDIF
159200020204     C                   ENDIF
159300020204     C*
159400020204     C                   IF        vabVMD > *zeros
159500150420     C                   IF        vabVAD <> *blanks
159600150420     C                   EVAL      vabVAD =  'EUR'
159700020204     C                   ENDIF
159800020204     C                   ENDIF
159900020204     C*
160000020204     C                   ENDSR
160100020204     C***
160200010330
160300010330
160400010330     C*----------------------------------------------------*
160500010330     C*  CONTROLLO NUMERICITA' CAMPI
160600010330     C*----------------------------------------------------*
160700010330     C     CHKNUM        BEGSR
160800010330     C*
160900010606     C                   IF        PiDecChr = *blanks
161000030715     C                   EVAL      PiDecChr = CharNUM
161100010606     C                   ENDIF
161200010606     C*
161300010606     C                   CALL(e)   'ISNUMERIC'
161400010330     C                   PARM                    PiStr            30
161500010606     C                   PARM                    PiDecChr          1
161600010330     C                   PARM      *ZEROS        PiVal            30 9
161700010330     C                   PARM      '0'           PiInt             1
161800010330     C                   PARM      '0'           PiNum             1
161900010330     C                   IF        %error
162000010606     C                   EVAL      PiNum=*off
162100010330     C                   ENDIF
162200010330     C*
162300010330     C                   ENDSR
162400010330     C***
162500160906
162600160906
162700160906      /TITLE Valorizzazione Progressivo Applicazione
162800160906     C     calprog       begsr
162900160906     C*
163000160906     C                   movel     *blanks       dwlisv            2
163100160906     C                   movel     *all'0'       dwlprg           10
163200160906     C                   z-add     *zeros        wrkprg            8 0
163300160906     C*
163400160906     C                   eval      dwlisv = vlrisv
163500160906     C*
163600170324     C     *start        setll     tis7prgf
163700160906     C                   read(e)   tis7prgf
163800160906     C                   if        not %error
163900160906     C                   eval      dwlprg = f_tis7prgf
164000160906     C*
164100160906     C                   move(p)   dwlprg        wrkprg
164200160906     C                   add       1             wrkprg
164300160906     C                   move(p)   wrkprg        dwlprg
164400160906     C                   movel     dwlisv        dwlprg
164500160906     C*
164600160906     C                   eval      f_tis7prgf = dwlprg
164700160906     C                   update    tis7prg0
164800170324     C                   else
164900170324     C                   eval      f_tis7prgf = 'XX00000000'
165000160906     C                   endif
165100160906     C*
165200160906     C                   endsr
165300050421
165400160908
165500160908     C     *pssr         BEGSR
165600160908     C*
165700160908     C                   if        %open(tivin00r)
165800160908     C                   close     tivin00r
165900160908     C                   endif
166000160908     C                   if        %open(fivabwwr)
166100160908     C                   close     fivabwwr
166200160908     C                   endif
166300160908     C                   if        %open(fivatwwr)
166400160908     C                   close     fivatwwr
166500160908     C                   endif
166600160908     C*
166700160908     C* Effettuo la chiamata al CLLE preposto
166800160908     C  N50              call(e)   'TITVVTC'
166900160908     C                   parm                    parccm
167000160908     C                   parm                    parmbr
167100160908     C                   parm      '2'           paropz
167200160908     C*
167300160908     C                   eval      wrkesito = '2'
167400160908     C*
167500160908     C                   seton                                        LR
167600160908     C*
167700160908     C                   ENDSR     '*CANCL'
167800160908     C***
167900100419
168000100419
168100160902     C     normCSV       BEGSR
168200160902     C*
168300160902     C                   z-add     *zeros        wLenVINDTA        4 0
168400160902     C                   z-add     *zeros        wFlgVINDTA        1 0
168500160902     C                   setoff                                       80
168600160902     C*
168700160902     C                   dow       wLenVINDTA < %len(vindta)
168800160902     C                   eval      wLenVINDTA = wLenVINDTA + 1
168900160902     C                   movel     *blanks       wAlfa1            1
169000160902     C                   eval      wAlfa1 = %subst(vindta:wLenVINDTA:1)
169100160902     C*
169200160902     C* Se byte inferiore a HEX40 è un byte "non stampabile" per cui considero *blank
169300160902     C                   if        wAlfa1 < X'40'
169400160902     C                   eval      %subst(vindta:wLenVINDTA:1) = *blanks
169500160902     C                   endif
169600160902     C*
169700160902     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
169800160902     C                   if        wAlfa1 <> CharCSV and
169900160902     C                             wAlfa1 <> CharTXT and
170000160902     C                             wAlfa1 <> CharNUM
170100160902     C                   z-add     1             wFlgVINDTA
170200160902     C                   endif
170300160902     C*
170400160902     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
170500160902     C* separatori campi
170600160902     C                   if        CharTXT <> *blanks
170700160902     C                   if        wAlfa1 = CharTXT
170800160902     C                   if        not *in80
170900160902     C                   seton                                        80
171000160902     C                   else
171100160902     C                   setoff                                       80
171200160902     C                   endif
171300160902     C                   endif
171400160902     C                   if        wAlfa1 = CharCSV and *in80 = *on
171500160902     C                   eval      %subst(vindta:wLenVINDTA:1) = CharSOS
171600160902     C                   endif
171700160902     C                   endif
171800160902     C*
171900160902     C                   enddo
172000100419     C*
172100100419     C                   ENDSR
172200100419     C***
172300150420
172400150420
172500150420      /TITLE Scrittura record in file TIVGD00F (file VAS generico download)
172600160902     C     wriVAB_VGD    BEGSR
172700160909     C*
172800160909     C* Verifica campi "particolari" VAB
172900160909     C                   exsr      repNSP
173000160909     C*
173100160909     C                   setoff                                       45
173200160909     C                   if        depSPE  = *zeros OR
173300160909     C                             depSPE <> vabNSP
173400160909     C                   seton                                        45
173500160909     C                   eval      depSPE  = vabNSP
173600160909     C                   endif
173700160909     C*
173800160909     C                   if        not *in31
173900150420     C*
174000160902     C* Se primo giro allora scrivo testata
174100160902     C                   if        wVGD_Head = *zeros
174200160902     C                   eval      wVGD_Head = 1
174300160902     C                   eval      wBufferOut =
174400170914     C                             %trim(pptCHR)+'VABATB' +%trim(pptCHR)+pptCSV+
174500160902     C                             %trim(pptCHR)+'VABCCM' +%trim(pptCHR)+pptCSV+
174600160902     C                             %trim(pptCHR)+'VABLNP' +%trim(pptCHR)+pptCSV+
174700160902     C                             %trim(pptCHR)+'VABCTR' +%trim(pptCHR)+pptCSV+
174800160902     C                             %trim(pptCHR)+'VABRSD' +%trim(pptCHR)+pptCSV+
174900160902     C                             %trim(pptCHR)+'VABRD2' +%trim(pptCHR)+pptCSV+
175000160902     C                             %trim(pptCHR)+'VABIND' +%trim(pptCHR)+pptCSV+
175100160902     C                             %trim(pptCHR)+'VABLOD' +%trim(pptCHR)+pptCSV+
175200160902     C                             %trim(pptCHR)+'VABCAD' +%trim(pptCHR)+pptCSV+
175300160902     C                             %trim(pptCHR)+'VABPRD' +%trim(pptCHR)+pptCSV+
175400160902     C                             %trim(pptCHR)+'VABCBO' +%trim(pptCHR)+pptCSV+
175500160902     C                             %trim(pptCHR)+'VABRMO' +%trim(pptCHR)+pptCSV+
175600160902     C                             %trim(pptCHR)+'VABRMN' +%trim(pptCHR)+pptCSV+
175700160902     C                             %trim(pptCHR)+'VABRMA' +%trim(pptCHR)+pptCSV+
175800160902     C                             %trim(pptCHR)+'VABNAS' +%trim(pptCHR)+pptCSV+
175900160902     C                             %trim(pptCHR)+'VABNCL' +%trim(pptCHR)+pptCSV+
176000160902     C                             %trim(pptCHR)+'VABPKB' +%trim(pptCHR)+pptCSV+
176100160902     C                             %trim(pptCHR)+'VABCAS' +%trim(pptCHR)+pptCSV+
176200160902     C                             %trim(pptCHR)+'VABVCA' +%trim(pptCHR)+pptCSV+
176300160902     C                             %trim(pptCHR)+'VABFFD' +%trim(pptCHR)+pptCSV+
176400160902     C                             %trim(pptCHR)+'VABIAS' +%trim(pptCHR)+pptCSV+
176500160902     C                             %trim(pptCHR)+'VABVAS' +%trim(pptCHR)+pptCSV+
176600160902     C                             %trim(pptCHR)+'VABNOT' +%trim(pptCHR)+pptCSV+
176700160902     C                             %trim(pptCHR)+'VABNRC' +%trim(pptCHR)+pptCSV+
176800160902     C                             %trim(pptCHR)+'VABTRC' +%trim(pptCHR)+pptCSV+
176900160902     C                             %trim(pptCHR)+'VABEMD' +%trim(pptCHR)+pptCSV+
177000160902     C                             %trim(pptCHR)+'VABCEL' +%trim(pptCHR)
177100160902     C                   exsr      wriVGD
177200160902     C                   endif
177300160902     C*
177400160902     C* Quindi scrivo dati spedizione
177500160909     C   45              eval      wBufferOut =
177600170914     C                             %trim(pptCHR)+%trim(VABATB)
177700170914     C                                                    +%trim(pptCHR)+pptCSV+
177800150420     C                             %trim(pptCHR)+%trim(%editc(VABCCM:'X'))
177900150420     C                                                    +%trim(pptCHR)+pptCSV+
178000150420     C                             %trim(pptCHR)+%trim(%editc(VABLNP:'X'))
178100150420     C                                                    +%trim(pptCHR)+pptCSV+
178200150420     C                             %trim(pptCHR)+%trim(%editc(VABCTR:'X'))
178300150420     C                                                    +%trim(pptCHR)+pptCSV+
178400150420     C                             %trim(pptCHR)+%trim(VABRSD)
178500150420     C                                                    +%trim(pptCHR)+pptCSV+
178600160902     C                             %trim(pptCHR)+%trim(VABRD2)
178700160902     C                                                    +%trim(pptCHR)+pptCSV+
178800150420     C                             %trim(pptCHR)+%trim(VABIND)
178900150420     C                                                    +%trim(pptCHR)+pptCSV+
179000150420     C                             %trim(pptCHR)+%trim(VABLOD)
179100150420     C                                                    +%trim(pptCHR)+pptCSV+
179200150420     C                             %trim(pptCHR)+%trim(VABCAD)
179300150420     C                                                    +%trim(pptCHR)+pptCSV+
179400150420     C                             %trim(pptCHR)+%trim(VABPRD)
179500150420     C                                                    +%trim(pptCHR)+pptCSV+
179600150420     C                             %trim(pptCHR)+%trim(VABCBO)
179700150420     C                                                    +%trim(pptCHR)+pptCSV+
179800150420     C                             %trim(pptCHR)+%trim(VABRMO)
179900150420     C                                                    +%trim(pptCHR)+pptCSV+
180000150420     C                             %trim(pptCHR)+%trim(%editc(VABRMN:'4'))
180100150420     C                                                    +%trim(pptCHR)+pptCSV+
180200150420     C                             %trim(pptCHR)+%trim(VABRMA)
180300150420     C                                                    +%trim(pptCHR)+pptCSV+
180400150420     C                             %trim(pptCHR)+%trim(VABNAS)
180500150420     C                                                    +%trim(pptCHR)+pptCSV+
180600150420     C                             %trim(pptCHR)+%trim(%editc(VABNCL:'4'))
180700150420     C                                                    +%trim(pptCHR)+pptCSV+
180800150420     C                             %trim(pptCHR)+%trim(%editc(VABPKB:'4'))
180900150420     C                                                    +%trim(pptCHR)+pptCSV+
181000150420     C                             %trim(pptCHR)+%trim(%editc(VABCAS:'4'))
181100150420     C                                                    +%trim(pptCHR)+pptCSV+
181200150420     C                             %trim(pptCHR)+%trim(VABVCA)
181300150420     C                                                    +%trim(pptCHR)+pptCSV+
181400150420     C                             %trim(pptCHR)+%trim(VABFFD)
181500150420     C                                                    +%trim(pptCHR)+pptCSV+
181600150420     C                             %trim(pptCHR)+%trim(%editc(VABIAS:'4'))
181700150420     C                                                    +%trim(pptCHR)+pptCSV+
181800150420     C                             %trim(pptCHR)+%trim(VABVAS)
181900150420     C                                                    +%trim(pptCHR)+pptCSV+
182000150421     C                             %trim(pptCHR)+%trim(VABNOT)
182100150421     C                                                    +%trim(pptCHR)+pptCSV+
182200160902     C                             %trim(pptCHR)+%trim(wVATNOT_A)
182300160902     C                                                    +%trim(pptCHR)+pptCSV+
182400160902     C                             %trim(pptCHR)+%trim(wVATNOT_B)
182500160902     C                                                    +%trim(pptCHR)+pptCSV+
182600150420     C                             %trim(pptCHR)+%trim(wVATNOT_I)
182700150420     C                                                    +%trim(pptCHR)+pptCSV+
182800150420     C                             %trim(pptCHR)+%trim(wVATNOT_S)
182900150420     C                                                    +%trim(pptCHR)
183000160909     C   45              exsr      wriVGD
183100160909     C*
183200160909     C                   endif
183300150420     C*
183400150420     C                   ENDSR
183500150420     C*------------------------------------------------------------------------*
183600150420
183700150420
183800150420
183900150420      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
184000150420     C     wriVGD        BEGSR
184100150420     C*
184200150420     C                   clear                   tivgd000
184300150420     C                   eval      vgdDTA = wBufferOut
184400150420     C                   eval      vgdTIP = 'EB'
184500150420     C                   eval      vgdKSU = vlrKSC
184600150420     C                   eval      vgdTSC = 'WW'
184700150420     C                   eval      vgdDAT = datcor
184800160906     C                   eval      vgdPRG = f_tis7prgf
184900160920     C                   eval      vgdPGM = procname
185000160906     C                   eval      vgdSTO = '?'
185100150420     C                   write     tivgd000
185200150420     C*
185300150420     C                   ENDSR
185400150420     C*------------------------------------------------------------------------*
185500160902
185600160902
185700160902     C*----------------------------------------------------*
185800160902     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
185900160902     C*----------------------------------------------------*
186000160902     C     SR_SPLIT      BEGSR
186100160902     C*
186200160902     C* Chiamo il *pgm d utilità x lo "split" della riga delle intestazioni
186300160902     C                   CALL      'XSPLIT2'
186400160902     C                   PARM                    InStringa
186500160902     C                   PARM                    InSepara
186600160902     C                   PARM      '5'           InTypeOut
186700160902     C                   PARM      ''            wSkParam
186800160902     C                   PARM                    OutErrore
186900160902     C                   PARM                    OutMsg
187000160902     C                   MOVEA     wSkParam      SkSplitCSV_5
187100160902     C*
187200160902     C                   ENDSR
187300150420
187400150420
187500000613     C     *inzsr        BEGSR
187600990910     C*
187700990910     C     *entry        plist
187800990920     C                   parm                    tivlrds
187900990921     C                   parm      wrkesito      esito
188000000724     C                   parm                    prmlit
188100000710     C                   parm                    prmfir
188200010330     C*
188300010330     C* CALCOLA LA DATA CORRENTE
188400150420     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
188500150420     C                   eval      datcor = %dec(%date() : *ISO)
188600000613     C*
188700000613     C                   ENDSR
188800000613     C***
188900160920** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
189000170914
189100170914
189200160920
189300160920
189400160920
