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
001200010330     D*----------------------------------------------------
001300010330     D* DICHIARAZIOINE VARIABILI DI WRK
001400010330     D*----------------------------------------------------
001500160908     D dscmz         e ds                  inz
001600010330     D psds           sds
001700010330     D  procname         *PROC
001800010330     D tivlrds       e ds                  extname(tivlr00f)
001900140114     D tisi95ds      e ds
002000010330     D esito           s              1
002100010330     D prmlit          s             10
002200010330     D prmfir          s             10
002300150420     D pptCHR          s              1    inz
002400150420     D pptCSV          s              1    inz
002500160902     D wBufferOut      s           2048    inz
002600160902     D wVATNOT_A       s             35    inz
002700160902     D wVATNOT_B       s             35    inz
002800160902     D wVATNOT_E       s             35    inz
002900150420     D wVATNOT_I       s             70    inz
003000160908     D wVATNOT_J       s             70    inz
003100150420     D wVATNOT_S       s             35    inz
003200160902     D wVABNOT         s             35    inz
003300160906     D wVABCTR_E       s                   like(VABCTR) inz
003400150420     D wrkesito        s                   like(esito)
003500160902     D wVGD_Head       s              1  0 inz(*zeros)
003600160902     D wFlgCAS         s              1  0 inz(*zeros)
003700160902     D s_vindta        s                   like(vindta) inz(*blanks)
003800160908     D parccm          s              8    inz(*blanks)
003900160908     D parmbr          s             10    inz(*blanks)
004000160908     D paropz          s              1    inz(*blanks)
004100160908     D chkcall         s              1    inz(*blanks)
004200160909     D depSPE          s              7  0 inz
004300160902
004400160908
004500160908     D*------------------
004600160908     D* DS REPERIMENTO NUMERATORE
004700160908     D*------------------
004800160908     D trul33ds      e ds                  inz
004900160908     D*------------------
005000160908     D* DS ARCHITETTURA
005100160908     D*------------------
005200160908     D KPJBA         e ds                  inz
005300160908
005400160902
005500160902     D*------------------
005600160902     D* VARIABILI X LA CHIAMATA AL *PGM UTILITA' "XSPLIT"
005700160902     D*------------------
005800160902     D InStringa       S          65535A   VARYING                              (stringa input)
005900160902     D InSepara        S             10A                                        (separatore)
006000160902     D InTypeOut       S              1A                                        (tipo output)
006100160902     D wSkParam        S          65535A                                        (output)
006200160902     D OutErrore       S              1A                                        (flag errore)
006300160902     D OutMsg          S             80A                                        (messaggio errore)
006400160902     D SkSplitCSV_5    S            256    DIM(256)
006500160902
006600160902
006700030715     D*------------------
006800030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
006900030715     D*------------------
007000160902     D SkSplitFLD      S                   LIKE(SkSplitCSV_5) DIM(256)
007100160902     D SkSplitCSV      S                   LIKE(SkSplitCSV_5) DIM(256)
007200160902     D CharCSV         S              1
007300160902     D CharTXT         S              1
007400160902     D CharNUM         S              1
007500160902     D CharSOS         S              1
007600160902     D i               s              3  0 INZ(1)
007700030715     D wGiro           s              1  0 INZ(*zeros)
007800030822     D*------------------
007900030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
008000030822     D*------------------
008100160902     D posDaDft        S              3  0 INZ(*zeros)
008200160902     D posADft         S              3  0 INZ(*zeros)
008300160902     D wGiroDft        s              1  0 INZ(*zeros)
008400160908
008500160908
008600031201     D*------------------
008700031201     D* Costanti
008800031201     D*------------------
008900160902     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
009000160908     D MAIU            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
009100150420
009200010330
009300010330
009400990921     C                   reset                   esito
009500990921     C                   reset                   wrkesito
009600010601     C*
009700160908     C                   exsr      defcam
009800160908     C   50              exsr      calprog
009900150420     C                   exsr      rwfile
010000160908     C   50              exsr      strvgd
010100160906     C                   exsr      endela
010200160906     C*
010300010601     C                   seton                                        lr
010400160906
010500160906
010600160906     C*--------------------------------------------------------
010700160906     C* STRVGD - "LIBERO" LA TRANSAZIONE (GRUPPO) CORRENTE    *
010800160906     C*--------------------------------------------------------
010900160906     C     STRVGD        BEGSR
011000160906     C*
011100160906     C
011200170119     C/EXEC SQL
011300170227     C+ UPDATE TIVGD00F SET VGDSTO = 'R'
011400170119     C+ WHERE VGDSTO = '?'     AND VGDPRG = :VGDPRG AND
011500170119     C+       VGDTIP = :VGDTIP AND VGDKSU = :VGDKSU AND
011600170119     C+       VGDTSC = :VGDTSC
011700170119     C/END-EXEC
011800160906     C
011900160906     C*
012000160906     C                   ENDSR
012100160906     C***
012200160902
012300010601
012400010601     C*--------------------------------------------------------
012500160902     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
012600010601     C*--------------------------------------------------------
012700010601     C     ENDELA        BEGSR
012800080222     C*
012900080222     C* Effettuo lancio TISI95 solo x chiusura
013000080222     C                   CLEAR                   TISI95DS
013100080222     C                   EVAL      I95TLA = 'C'
013200080222     C                   CALL      'TISI95R'
013300080222     C                   PARM                    TISI95DS
013400160908     C*
013500160908     C* Effettuo la chiamata al CLLE preposto
013600160908     C  N50              call(e)   'TITVVTC'
013700160908     C                   parm                    parccm
013800160908     C                   parm                    parmbr
013900160908     C                   parm      '2'           paropz
014000000613     C*
014100010601     C                   ENDSR
014200010601     C***
014300010601
014400010601
014500010330     C*--------------------------------------------------------
014600150420     C* RWFILE  LEGGE TIVIN00R E SCRIVE FIVABWWF              *
014700010330     C*--------------------------------------------------------
014800150420     C     RWFILE        BEGSR
014900010330     C*
015000010330     C                   if        not %open(tivin00r)
015100010330     C                   open      tivin00r
015200010330     C                   endif
015300160908     C*
015400160908     C* Eseguo operazioni di aggiunta nuovo membro file FNVAT
015500160908     C  N50              exsr      prevat
015600160908     C   50              eval      chkcall = '0'
015700160908     C*
015800160908     C                   if        chkcall = '0'
015900160908     C*
016000160908     C                   if        not %open(fivabwwr)
016100160908     C                   open      fivabwwr
016200160908     C                   endif
016300160908     C*
016400160908     C                   if        not %open(fivatwwr)
016500160908     C                   open      fivatwwr
016600160908     C                   endif
016700010330     C*
016800160908     C                   clear                   §CTROKVB          7 0
016900160908     C                   clear                   §CTROKVT          7 0
017000160908     C                   clear                   §CTRMO            7 0
017100160908     C                   clear                   §CTRNO            7 0
017200010330     C*
017300030822     C                   do        *HIVAL
017400010330     C*
017500030822     C                   read      tivin00r                               70
017600160902     C*
017700160902     C* Salvo il buffer orginale ricevuto dal cliente
017800160902     C                   eval      s_vindta = vindta
017900160902     C*
018000160902     C                   if        *in70 = *off and
018100160902     C                             vindta <> *blanks and
018200160902     C                             (vinflg = *blanks
018300160902     C                              or vinflg = '0'
018400160902     C                              or vinflg = '2')
018500160902     C*
018600160902     C* Controlli di "base" sui dati grezzi
018700160902     C                   exsr      normCSV
018800040510     C*
018900160902     C                   if        wFlgVINDTA = 1
019000010330     C*
019100010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
019200010711     C                   if        vinflg = *blanks or vinflg = '0'
019300010711     C                   clear                   vinmsg
019400010711     C                   endif
019500160902     C*
019600160902 xxx C* Porto eventualmente da minuscolo in maiuscolo le INTESTAZIONI colonne cliente
019700160908 xxx C***  minu:MAIU     XLATE     vindta        vindta
019800160902     C*
019900160902     C* Splitto i dati
020000160902     C                   CLEAR                   SkSplitCSV_5
020100160902     C                   EVAL      InStringa = vindta
020200160902     C                   EVAL      InSepara  = CharCSV
020300160902     C                   EXSR      SR_SPLIT
020400160902     C*
020500160902     C*****************************
020600160916     C* *PERS (consigliato test almeno su 3 colonne)
020700160916     C* A questo punto verifico che le colonne mandatorie siano valorizzate correttamente
020800160909     C                   if        %len(%trim(SkSplitCSV_5(2)))  > 0 and
020900160916     C                             %len(%trim(SkSplitCSV_5(3)))  > 0 and
021000160916     C                             %len(%trim(SkSplitCSV_5(10))) > 0
021100160902     C*****************************
021200010601     C*
021300160902     C* Se OK => procedo con elaborazione
021400010330     C                   exsr      inzvar
021500010605     C                   exsr      impvab
021600010601     C*
021700010604     C* Ebbene...
021800160908     C                   if        wGiro > 1
021900160908     C  N31              add       1             §CTROKVB
022000160908     C   32              add       1             §CTRMO
022100160908     C   31              add       1             §CTRNO
022200160908     C*
022300160902     C   50              exsr      wriVAB_VGD
022400160908     C  N50              exsr      wriVAB_WW
022500150420     C                   endif
022600010604     C*
022700010604     C                   if        *in31 = *off and
022800010604     C                             *in32 = *off
022900010604     C                   eval      vinflg = '1'
023000010604     C                   else
023100010604     C                   eval      vinflg = '2'
023200010604     C                   endif
023300010601     C*
023400160902     C                   else
023500160902     C                   eval      vinflg = '1'
023600010604     C                   endif
023700160902     C*
023800160902     C                   else
023900160902     C                   eval      vinflg = '1'
024000160902     C                   endif
024100010604     C*
024200010330     C                   else
024300010330     C                   eval      vinflg = '1'
024400010330     C                   endif
024500010601     C*
024600160902     C                   eval      vindta = s_vindta
024700010601     C  N70              update    tivin000
024800010330     C*
024900030822     C  N70              enddo
025000010601     C
025100010601     C*
025200160902     C* Stabilisco l'esito della traduzione
025300160902     C                   select
025400160902     C*
025500160902     C* Se non ci sono record con errori => stato OK
025600160902     C                   when      §ctrno = 0 and
025700160902     C                             §ctrmo = 0
025800010330     C                   eval      wrkesito = '0'
025900160902     C*
026000160902     C* Se nessun record OK => errore totale
026100160908     C                   when      §ctrokvb = 0
026200160902     C                   eval      wrkesito = '2'
026300160902     C*
026400160902     C* Diversamente => errore parziale/warning
026500160902     C                   other
026600160902     C                   eval      wrkesito = '1'
026700160902     C*
026800160902     C                   endsl
026900160908     C*
027000160908     C                   endif
027100160908     C
027200160908     C                   if        chkcall = '1'
027300160908     C                   eval      wrkesito = '2'
027400160908     C                   endif
027500010330     C*
027600010330     C                   if        %open(tivin00r)
027700010330     C                   close     tivin00r
027800010330     C                   endif
027900160908     C*
028000160908     C                   if        %open(fivabwwr)
028100160908     C                   close     fivabwwr
028200160908     C                   endif
028300160908     C                   if        %open(fivatwwr)
028400160908     C                   close     fivatwwr
028500160908     C                   endif
028600160908     C*
028700160908     C                   if        not *in50        and
028800160908     C                             wrkesito <> '2'  and
028900160908     C                             vlrpoi <> *zeros
029000160908     C                   exsr      invio
029100160908     C                   endif
029200010330     C*
029300010330     C                   ENDSR
029400010330     C***
029500160908
029600160908
029700160908     C*----------------------------------------------------*
029800160908     C*  SCARICAMENTO BUFFER RECORDS VAB IN "WW"
029900160908     C*----------------------------------------------------*
030000160908     C     WRIVAB_WW     BEGSR
030100160908     C*
030200160908     C* Verifica campi "particolari" VAB
030300160908     C                   exsr      repNSP
030400160908     C*
030500160909     C                   setoff                                       45
030600160909     C                   if        depSPE  = *zeros OR
030700160909     C                             depSPE <> vabNSP
030800160909     C                   seton                                        45
030900160909     C                   eval      depSPE  = vabNSP
031000160909     C                   endif
031100160909     C*
031200160909     C                   if        not *in31
031300160909     C                   exsr      EXEVATE
031400160909     C   45              exsr      EXEVATA
031500160909     C   45              exsr      EXEVATB
031600160909     C   45              exsr      EXEVATI
031700160909     C   45              exsr      EXEVATJ
031800160909     C   45              exsr      EXEVATS
031900160909     C   45              write     fivab000                                     => scarico il VAB
032000160908     C                   endif
032100160908     C*
032200160908     C                   ENDSR
032300160908
032400160908
032500160908     C*----------------------------------------------------*
032600160908     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
032700160908     C*----------------------------------------------------*
032800160908     C     REPNSP        BEGSR
032900160908     C*
033000160909     C* NSP => Se necessario stacco un numeratore da AZNUM
033100160909     C                   if        VABNSP = *zeros
033200160908     C                   clear                   TRUL33DS
033300160908     C                   eval      I33OPE = *zeros
033400160908     C                   eval      I33CNU = 302
033500160908     C                   eval      I33NUM = 1
033600160908     C                   movel     TRUL33DS      KPJBU
033700160908     C                   call      'TRUL33R'
033800160908     C                   parm                    KPJBA
033900160908     C                   movel     KPJBU         TRUL33DS
034000160908     C                   if        O33ERR = *zeros
034100160908     C                   z-add     O33NRF        VABNSP
034200160908     C                   z-add     O33NRF        VATNSP
034300160908     C                   else
034400160908     C                   SETON                                        31
034500160908     C                   EVAL      vinmsg = %trimr(vinmsg)
034600160908     C                             + ' ' + 'VABNSP VATNSP'
034700160908     C                   endif
034800160909     C                   endif
034900160908     C*
035000160908     C                   ENDSR
035100160908
035200160908
035300160908     C*----------------------------------------------------*
035400160908     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
035500160908     C*----------------------------------------------------*
035600160908     C     EXEVATA       BEGSR
035700160908     C*
035800160908     C                   EVAL      VATTRC = 'A'
035900160908     C                   EVAL      VATNOT = wVATNOT_A
036000160908     C                   exsr      wrivat                                       => scarico VAT
036100160908     C*
036200160908     C                   ENDSR
036300160908     C*----------------------------------------------------*
036400160908     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
036500160908     C*----------------------------------------------------*
036600160908     C     EXEVATB       BEGSR
036700160908     C*
036800160908     C                   EVAL      VATTRC = 'B'
036900160908     C                   EVAL      VATNOT = wVATNOT_B
037000160908     C                   exsr      wrivat                                       => scarico VAT
037100160908     C*
037200160908     C                   ENDSR
037300160909     C*----------------------------------------------------*
037400160909     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "I"
037500160909     C*----------------------------------------------------*
037600160909     C     EXEVATI       BEGSR
037700160909     C*
037800160909     C                   IF        %len(%trim(wVATNOT_I)) > 35
037900160909     C                   EVAL      VATTRC = 'I'
038000160909     C                   EVAL      VATNOT = %subst(%trim(wVATNOT_I):1:35)
038100160909     C                   exsr      wrivat                                       => scarico VAT
038200160909     C                   EVAL      VATTRC = 'J'
038300160909     C                   EVAL      VATNOT = %subst(%trim(wVATNOT_I):36)
038400160909     C                   exsr      wrivat                                       => scarico VAT
038500160909     C                   ELSE
038600160909     C                   EVAL      VATTRC = 'I'
038700160909     C                   EVAL      VATNOT = wVATNOT_I
038800160909     C                   exsr      wrivat                                       => scarico VAT
038900160909     C                   ENDIF
039000160909     C*
039100160909     C                   ENDSR
039200160909     C*----------------------------------------------------*
039300160909     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "J"
039400160909     C*----------------------------------------------------*
039500160909     C     EXEVATJ       BEGSR
039600160909     C*
039700160909     C                   IF        %len(%trim(wVATNOT_I)) <= 35
039800160909     C                   EVAL      VATTRC = 'J'
039900160909     C                   EVAL      VATNOT = wVATNOT_J
040000160909     C                   exsr      wrivat                                       => scarico VAT
040100160909     C                   ENDIF
040200160909     C*
040300160909     C                   ENDSR
040400160908     C*----------------------------------------------------*
040500160908     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "S"
040600160908     C*----------------------------------------------------*
040700160908     C     EXEVATS       BEGSR
040800160908     C*
040900160908     C                   EVAL      VATTRC = 'S'
041000160908     C                   EVAL      VATNOT = wVATNOT_S
041100160908     C                   exsr      wrivat                                       => scarico VAT
041200160908     C*
041300160908     C                   ENDSR
041400160908     C*----------------------------------------------------*
041500160908     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
041600160908     C*----------------------------------------------------*
041700160916     C     EXEVATE       BEGSR
041800160916     C*
041900160916     C***                EVAL      VATTRC = 'E'
042000160916     C***                EVAL      VATNOT = wVATNOT_E
042100160916     C***                exsr      wrivat                                       => scarico VAT
042200160916     C*****************************
042300160916     C* *PERS
042400160916     C                   EVAL      VATTRC = 'E'
042500160916     C* Sviluppo i "chi sono" in relazione al numero di colli della spedizione
042600160916     C                   MOVEL     *zeros        wNumCol           3 0
042700160916     C                   MOVEL     *blanks       wNumDoc           6
042800160916     C                   EVAL      wNumDoc = %subst(wVATNOT_E:5:6)
042900160923     C                   DOW       wNumCol < VABNCL AND wNumCol < 999
043000160920     C                   ADD       1             wNumCol
043100160923     C                   EVAL      VATNOT = %trim(VABRMA) +
043200160923     C                                      wNumDoc + %editc(wNumCol:'X')
043300160916     C                   exsr      wrivat                                       => scarico VAT
043400160916     C                   ENDDO
043500160916     C*****************************
043600160908     C*
043700160908     C                   ENDSR
043800160908     C*----------------------------------------------------*
043900160908     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
044000160908     C*----------------------------------------------------*
044100160908     C     WRIVAT        BEGSR
044200160908     C*
044300160908     C* Scrivo solo se valorizzato qualcosa
044400160908     C                   IF        VATNOT <> *blanks
044500160908     C                   EVAL      VATFGS = VABFGS
044600160908     C                   EVAL      VATCCM = VABCCM
044700160908     C                   EVAL      VATLNP = VABLNP
044800160908     C                   EVAL      VATAAS = VABAAS
044900160908     C                   EVAL      VATNRS = VABNRS
045000160908     C                   EVAL      VATNSP = VABNSP
045100160908     C                   WRITE     FIVAT000
045200160908     C                   ADD       1             §CTROKVT
045300160908     C                   ENDIF
045400160908     C*
045500160908     C                   ENDSR
045600160908
045700160908
045800160908     C*----------------------------------------------------*
045900160908     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE ACCESSORIO
046000160908     C*----------------------------------------------------*
046100160908     C     PREVAT        BEGSR
046200160908     C*
046300160908     C* Compongo il nome del membro da dare al FIVATWWR
046400160908     C                   eval      parmbr = vlrhdl
046500160908     C                   movel     'M'           parmbr
046600160908     C                   eval      parccm = %subst(vlrKSC:2:7)
046700160908     C                   eval      paropz = '1'
046800160908     C*
046900160908     C* Effettuo la chiamata al CLLE preposto
047000160908     C                   call(e)   'TITVVTC'
047100160908     C                   parm                    parccm
047200160908     C                   parm                    parmbr
047300160908     C                   parm                    paropz
047400160908     C*
047500160908     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
047600160908     C                   if        %error
047700160908     C                   movel     '1'           chkcall
047800160908     C                   else
047900160908     C                   movel     '0'           chkcall
048000160908     C                   endif
048100160908     C*
048200160908     C                   ENDSR
048300160908
048400160908
048500160908      /TITLE Invio dei dati al punto operativo.
048600160908     C     invio         BEGSR
048700160908     C*
048800160908     C* 1° invio FIVAT
048900160908     C                   reset                   dscmz
049000160908     C                   move      vlrpoi        cmzdst
049100160908     C                   eval      cmzfld = 'FIVATWWR'
049200160908     C                   eval      cmzmbd = vlrhdl
049300160908     C                   eval      %subst(cmzmbd:1:1) = 'M'
049400160908     C***                if        prmfir = *blanks
049500160908     C                   eval      cmzfla = 'FIVAT00F'
049600160908     C                   eval      cmzmba = 'FIVAT00F'
049700160908     C***                else
049800160908     C***                eval      cmzfla = prmfir
049900160908     C***                eval      cmzmba = prmfir
050000160908     C***                endif
050100160908     C                   eval      cmznrr = *zeros
050200160908     C                   move      §ctrokvt      cmznrr
050300160908     C                   eval      cmzlba = vlrfl1
050400160908     C                   call(e)   'TIS711C'
050500160908     C                   parm                    dscmz
050600160908     C                   parm      *blanks       esito
050700160908     C                   if        %error
050800160908     C                             or cmzerr = '1'
050900160908     C                             or esito  = '1'
051000160908     C                   eval      wrkesito = '3'
051100160908     C                   else
051200160908     C*
051300160908     C* 2° invio FIVAB
051400160908     C                   reset                   dscmz
051500160908     C                   move      vlrpoi        cmzdst
051600160908     C                   eval      cmzfld = vlrfou
051700160908     C                   eval      cmzmbd = vlrhdl
051800160908     C                   eval      %subst(cmzmbd:1:1) = 'M'
051900160908     C***                if        prmfir = *blanks
052000160908     C                   eval      cmzfla = 'FIVAB00F'
052100160908     C                   eval      cmzmba = 'FIVAB00F'
052200160908     C***                else
052300160908     C***                eval      cmzfla = prmfir
052400160908     C***                eval      cmzmba = prmfir
052500160908     C***                endif
052600160908     C                   eval      cmznrr = *zeros
052700160908     C                   move      §ctrokvb      cmznrr
052800160908     C                   eval      cmzlba = vlrfl1
052900160908     C                   call(e)   'TIS711C'
053000160908     C                   parm                    dscmz
053100160908     C                   parm      *blanks       esito
053200160908     C                   if        %error
053300160908     C                             or cmzerr = '1'
053400160908     C                             or esito  = '1'
053500160908     C                   eval      wrkesito = '3'
053600160908     C                   endif
053700160908     C                   endif
053800160908     C*
053900160908     C                   ENDSR
054000160908     C***
054100010601
054200010601
054300010330     C*----------------------------------------------------*
054400030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
054500010330     C*----------------------------------------------------*
054600010330     C     INZVAR        BEGSR
054700010330     C*
054800030822     C* Inizializzo variabili di wrk
054900160902     C                   Z-ADD     *zeros        Num5_0            5 0
055000030822     C*
055100030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
055200160908     C                   CLEAR                   FIVAB000
055300160908     C                   CLEAR                   FIVAT000
055400160908     C                   CLEAR                   wVABNOT
055500160902     C                   CLEAR                   wVATNOT_A
055600160902     C                   CLEAR                   wVATNOT_B
055700160902     C                   CLEAR                   wVATNOT_E
055800150420     C                   CLEAR                   wVATNOT_I
055900160908     C                   CLEAR                   wVATNOT_J
056000150420     C                   CLEAR                   wVATNOT_S
056100030822     C                   CLEAR                   SkSplitCSV
056200160902     C                   CLEAR                   wFlgCAS
056300030822     C                   EVAL      i = 1
056400030822     C*
056500030822     C* Reimposto i valori di default
056600030822     C                   EXSR      DEFCAM
056700030822     C*
056800010330     C                   ENDSR
056900160902
057000160902
057100010330     C*----------------------------------------------------*
057200030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
057300010330     C*----------------------------------------------------*
057400010330     C     DEFCAM        BEGSR
057500110706     C*
057600160906     C                   SETOFF                                       7172
057700030822     C*
057800030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
057900030715     C* e delimitatore testo.
058000030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
058100030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
058200030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
058300150420     C                   IF        %subst(vlrppt:5:1) <> *blanks
058400150420     C                   EVAL      pptCSV = %subst(vlrppt:5:1)
058500150420     C                   ELSE
058600150420     C                   EVAL      pptCSV = ';'
058700150420     C                   ENDIF
058800150420     C                   IF        %subst(vlrppt:6:1) <> *blanks
058900150420     C                   EVAL      pptCHR = %subst(vlrppt:6:1)
059000150420     C                   ELSE
059100150420     C                   EVAL      pptCHR = ' '
059200150420     C                   ENDIF
059300090403     C*
059400090403     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
059500160902     C                   EVAL      CharSOS = CharNUM
059600030822     C*
059700030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
059800030715     C                   IF        %subst(vlrppt:1:1) = 'S'
059900160902     C                   SETON                                        50        * OUT => Download
060000030715     C                   ELSE
060100160902     C                   SETOFF                                       50        * OUT => File WW
060200030715     C                   ENDIF
060300030822     C*
060400030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
060500030822     C                   EVAL      posDaDft = 1
060600030822     C                   EVAL      posADft  = 0
060700030822     C                   EVAL      wGiroDft = 0
060800030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
060900030822     C                             posDaDft > 0
061000030822     C*
061100030822     C* Gestisco il 1° giro
061200030822     C                   IF        wGiroDft = 0
061300030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
061400030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
061500030822     C* Incremento il contatore dei "giri"
061600030822     C                   EVAL      wGiroDft = 1
061700030822     C                   ELSE
061800030822     C                   EVAL      posDaDft = posADft
061900030822     C                   ENDIF
062000030822     C* Eseguo lo scan x trovare la fine del campo corrente
062100030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
062200030822     C*
062300030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
062400030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
062500030822     C                   IF        posDaDft > 0 AND
062600030822     C                             posADft  > 0
062700110706     C* RMN
062800110706     C                   IF        %subst(
062900110706     C                             %subst(vlrppt:posDaDft+1:
063000110706     C                             posADft-posDaDft-1):1:3)
063100110706     C                             = 'RMN'
063200110706     C                   IF        %trim(%subst(
063300110706     C                             %subst(vlrppt:posDaDft+1:
063400110706     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063500110706     C                             = '*NSP'
063600160902     C                   SETON                                        71
063700110706     C                   ENDIF
063800110706     C                   ENDIF
063900060331     C* FGS
064000060331     C                   IF        %subst(
064100060331     C                             %subst(vlrppt:posDaDft+1:
064200060331     C                             posADft-posDaDft-1):1:3)
064300060331     C                             = 'FGS'
064400060331     C                   EVAL      PiStr=%trim(%subst(
064500060331     C                             %subst(vlrppt:posDaDft+1:
064600060331     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064700060331     C                   EXSR      CHKNUM
064800060331     C                   IF        PiInt=*on
064900060331     C                   Z-ADD     PiVal         VABFGS
065000060331     C                   ENDIF
065100060331     C                   ENDIF
065200030822     C* NCL
065300030822     C                   IF        %subst(
065400030822     C                             %subst(vlrppt:posDaDft+1:
065500030822     C                             posADft-posDaDft-1):1:3)
065600030822     C                             = 'NCL'
065700030822     C                   EVAL      PiStr=%trim(%subst(
065800030822     C                             %subst(vlrppt:posDaDft+1:
065900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066000030822     C                   EXSR      CHKNUM
066100030822     C                   IF        PiInt=*on
066200030822     C                   Z-ADD     PiVal         VABNCL
066300030822     C                   ENDIF
066400030822     C                   ENDIF
066500030822     C* CCM
066600030822     C                   IF        %subst(
066700030822     C                             %subst(vlrppt:posDaDft+1:
066800030822     C                             posADft-posDaDft-1):1:3)
066900030822     C                             = 'CCM'
067000030822     C                   EVAL      PiStr=%trim(%subst(
067100030822     C                             %subst(vlrppt:posDaDft+1:
067200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067300030822     C                   EXSR      CHKNUM
067400030822     C                   IF        PiInt=*on
067500030822     C                   Z-ADD     PiVal         VABCCM
067600030822     C                   ENDIF
067700030822     C                   ENDIF
067800030822     C* LNP
067900030822     C                   IF        %subst(
068000030822     C                             %subst(vlrppt:posDaDft+1:
068100030822     C                             posADft-posDaDft-1):1:3)
068200030822     C                             = 'LNP'
068300030822     C                   EVAL      PiStr=%trim(%subst(
068400030822     C                             %subst(vlrppt:posDaDft+1:
068500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068600030822     C                   EXSR      CHKNUM
068700030822     C                   IF        PiInt=*on
068800030822     C                   Z-ADD     PiVal         VABLNP
068900030822     C                   ENDIF
069000030822     C                   ENDIF
069100030822     C* NRS
069200030822     C                   IF        %subst(
069300030822     C                             %subst(vlrppt:posDaDft+1:
069400030822     C                             posADft-posDaDft-1):1:3)
069500030822     C                             = 'NRS'
069600030822     C                   EVAL      PiStr=%trim(%subst(
069700030822     C                             %subst(vlrppt:posDaDft+1:
069800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069900030822     C                   EXSR      CHKNUM
070000030822     C                   IF        PiInt=*on
070100030822     C                   Z-ADD     PiVal         VABNRS
070200030822     C                   ENDIF
070300030822     C                   ENDIF
070400160906     C* CTR
070500030822     C                   IF        %subst(
070600030822     C                             %subst(vlrppt:posDaDft+1:
070700030822     C                             posADft-posDaDft-1):1:3)
070800030822     C                             = 'CTR'
070900030822     C                   EVAL      PiStr=%trim(%subst(
071000030822     C                             %subst(vlrppt:posDaDft+1:
071100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071200030822     C                   EXSR      CHKNUM
071300030822     C                   IF        PiInt=*on
071400160906     C                   Z-ADD     PiVal         VABCTR
071500030822     C                   ENDIF
071600030822     C                   ENDIF
071700160906     C* CTE (ovvero CTR per Estero)
071800160906     C                   IF        %subst(
071900160906     C                             %subst(vlrppt:posDaDft+1:
072000160906     C                             posADft-posDaDft-1):1:3)
072100160906     C                             = 'CTE'
072200160906     C                   EVAL      PiStr=%trim(%subst(
072300160906     C                             %subst(vlrppt:posDaDft+1:
072400160906     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072500160906     C                   EXSR      CHKNUM
072600160906     C                   IF        PiInt=*on
072700160906     C                   Z-ADD     PiVal         wVABCTR_E
072800160906     C                   SETON                                        72
072900160906     C                   ENDIF
073000160906     C                   ENDIF
073100030822     C* PKB
073200030822     C                   IF        %subst(
073300030822     C                             %subst(vlrppt:posDaDft+1:
073400030822     C                             posADft-posDaDft-1):1:3)
073500030822     C                             = 'PKB'
073600030822     C                   EVAL      PiStr=%trim(%subst(
073700030822     C                             %subst(vlrppt:posDaDft+1:
073800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073900030822     C                   EXSR      CHKNUM
074000030822     C                   IF        PiNum=*on
074100030822     C                   Z-ADD     PiVal         VABPKB
074200030822     C                   ENDIF
074300030822     C                   ENDIF
074400030822     C* VLB
074500030822     C                   IF        %subst(
074600030822     C                             %subst(vlrppt:posDaDft+1:
074700030822     C                             posADft-posDaDft-1):1:3)
074800030822     C                             = 'VLB'
074900030822     C                   EVAL      PiStr=%trim(%subst(
075000030822     C                             %subst(vlrppt:posDaDft+1:
075100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075200030822     C                   EXSR      CHKNUM
075300030822     C                   IF        PiNum=*on
075400030822     C                   Z-ADD     PiVal         VABVLB
075500030822     C                   ENDIF
075600030822     C                   ENDIF
075700030822     C* QFT
075800030822     C                   IF        %subst(
075900030822     C                             %subst(vlrppt:posDaDft+1:
076000030822     C                             posADft-posDaDft-1):1:3)
076100030822     C                             = 'QFT'
076200030822     C                   EVAL      PiStr=%trim(%subst(
076300030822     C                             %subst(vlrppt:posDaDft+1:
076400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076500030822     C                   EXSR      CHKNUM
076600030822     C                   IF        PiNum=*on
076700030822     C                   Z-ADD     PiVal         VABQFT
076800030822     C                   ENDIF
076900030822     C                   ENDIF
077000030822     C* CBO
077100030822     C                   IF        %subst(
077200030822     C                             %subst(vlrppt:posDaDft+1:
077300030822     C                             posADft-posDaDft-1):1:3)
077400030822     C                             = 'CBO'
077500030822     C                   EVAL      VABCBO=%trim(%subst(
077600030822     C                             %subst(vlrppt:posDaDft+1:
077700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077800030822     C                   ENDIF
077900030822     C* TSP
078000030822     C                   IF        %subst(
078100030822     C                             %subst(vlrppt:posDaDft+1:
078200030822     C                             posADft-posDaDft-1):1:3)
078300030822     C                             = 'TSP'
078400030822     C                   EVAL      VABTSP=%trim(%subst(
078500030822     C                             %subst(vlrppt:posDaDft+1:
078600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078700030822     C                   ENDIF
078800030822     C* VAS
078900030822     C                   IF        %subst(
079000030822     C                             %subst(vlrppt:posDaDft+1:
079100030822     C                             posADft-posDaDft-1):1:3)
079200030822     C                             = 'VAS'
079300030822     C                   EVAL      VABVAS=%trim(%subst(
079400030822     C                             %subst(vlrppt:posDaDft+1:
079500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079600030822     C                   ENDIF
079700030822     C* VCA
079800030822     C                   IF        %subst(
079900030822     C                             %subst(vlrppt:posDaDft+1:
080000030822     C                             posADft-posDaDft-1):1:3)
080100030822     C                             = 'VCA'
080200030822     C                   EVAL      VABVCA=%trim(%subst(
080300030822     C                             %subst(vlrppt:posDaDft+1:
080400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080500030822     C                   ENDIF
080600030822     C* GCA
080700030822     C                   IF        %subst(
080800030822     C                             %subst(vlrppt:posDaDft+1:
080900030822     C                             posADft-posDaDft-1):1:3)
081000030822     C                             = 'GCA'
081100030822     C                   EVAL      VABGCA=%trim(%subst(
081200030822     C                             %subst(vlrppt:posDaDft+1:
081300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081400030822     C                   ENDIF
081500030822     C* CTM
081600030822     C                   IF        %subst(
081700030822     C                             %subst(vlrppt:posDaDft+1:
081800030822     C                             posADft-posDaDft-1):1:3)
081900030822     C                             = 'CTM'
082000030822     C                   EVAL      VABCTM=%trim(%subst(
082100030822     C                             %subst(vlrppt:posDaDft+1:
082200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082300030822     C                   ENDIF
082400030822     C* FFD
082500030822     C                   IF        %subst(
082600030822     C                             %subst(vlrppt:posDaDft+1:
082700030822     C                             posADft-posDaDft-1):1:3)
082800030822     C                             = 'FFD'
082900030822     C                   EVAL      VABFFD=%trim(%subst(
083000030822     C                             %subst(vlrppt:posDaDft+1:
083100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
083200030822     C                   ENDIF
083300030822     C* VAD
083400030822     C                   IF        %subst(
083500030822     C                             %subst(vlrppt:posDaDft+1:
083600030822     C                             posADft-posDaDft-1):1:3)
083700030822     C                             = 'VAD'
083800030822     C                   EVAL      VABVAD=%trim(%subst(
083900030822     C                             %subst(vlrppt:posDaDft+1:
084000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
084100030822     C                   ENDIF
084200030822     C* GMA
084300030822     C                   IF        %subst(
084400030822     C                             %subst(vlrppt:posDaDft+1:
084500030822     C                             posADft-posDaDft-1):1:3)
084600030822     C                             = 'GMA'
084700030822     C                   EVAL      VABGMA=%trim(%subst(
084800030822     C                             %subst(vlrppt:posDaDft+1:
084900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
085000030822     C                   ENDIF
085100030822     C* GGA
085200030822     C                   IF        %subst(
085300030822     C                             %subst(vlrppt:posDaDft+1:
085400030822     C                             posADft-posDaDft-1):1:3)
085500030822     C                             = 'GGA'
085600030822     C                   EVAL      VABGGA=%trim(%subst(
085700030822     C                             %subst(vlrppt:posDaDft+1:
085800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
085900030822     C                   ENDIF
086000030822     C* GVA
086100030822     C                   IF        %subst(
086200030822     C                             %subst(vlrppt:posDaDft+1:
086300030822     C                             posADft-posDaDft-1):1:3)
086400030822     C                             = 'GVA'
086500030822     C                   EVAL      VABGVA=%trim(%subst(
086600030822     C                             %subst(vlrppt:posDaDft+1:
086700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
086800030822     C                   ENDIF
086900030822     C* TC1
087000030822     C                   IF        %subst(
087100030822     C                             %subst(vlrppt:posDaDft+1:
087200030822     C                             posADft-posDaDft-1):1:3)
087300030822     C                             = 'TC1'
087400030822     C                   EVAL      VABTC1=%trim(%subst(
087500030822     C                             %subst(vlrppt:posDaDft+1:
087600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
087700030822     C                   ENDIF
087800030822     C* TC2
087900030822     C                   IF        %subst(
088000030822     C                             %subst(vlrppt:posDaDft+1:
088100030822     C                             posADft-posDaDft-1):1:3)
088200030822     C                             = 'TC2'
088300030822     C                   EVAL      VABTC2=%trim(%subst(
088400030822     C                             %subst(vlrppt:posDaDft+1:
088500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
088600030822     C                   ENDIF
088700030822     C* ...
088800030822     C                   ENDIF
088900030822     C                   ENDDO
089000020204     C*
089100010330     C                   ENDSR
089200010607     C*----------------------------------------------------*
089300021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
089400010607     C*----------------------------------------------------*
089500010607     C     IMPVAB        BEGSR
089600010607     C*
089700010607     C                   SETOFF                                       3132
089800030515     C*
089900010607     C                   MOVEL     datcor        VABAAS
090000010607     C                   MOVE      datcor        VABMGS
090100160902     C*
090200160902     C* Dopo aver splittato nei vari campi eseguo 1 prima "normalizzazione" x eliminare
090300160902     C* i caratteri di delimitazione testo e gli apici
090400160902     C                   EVAL      SkSplitCSV = SkSplitCSV_5
090500160902     C                   EVAL      i = 1
090600160902     C                   DOW       i < %elem(SkSplitCSV)
090700160902     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
090800160902     C     '''':' '      XLATE     SkSplitCSV(i) SkSplitCSV(i)
090900160902     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
091000160902     C                   EVAL      i = i + 1
091100160902     C                   ENDDO
091200010607     C*
091300030715     C**********
091400160902     C* Valorizzo la lista delle INTESTAZIONI colonne cliente
091500030715     C**********
091600030715     C                   IF        wGiro = *zeros
091700160902     C*
091800160902     C                   EVAL      wGiro = 1
091900160902     C                   EVAL      SkSplitFLD = SkSplitCSV
092000160902     C*
092100160902     C                   ELSE
092200160902     C*
092300160902     C**********
092400160902     C* Elaboro le righe di DATI cliente
092500160902     C**********
092600160902     C                   EVAL      wGiro = 2
092700030715     C*
092800030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
092900030715     C                   EVAL      i = 1
093000030715     C                   DOW       i < %elem(SkSplitFLD)
093100030715     C*
093200030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
093300030715     C                   IF        SkSplitFLD(i) <> *blanks AND
093400030715     C                             SkSplitCSV(i) <> *blanks
093500130624     C*** ATB
093600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
093700150420     C                   EVAL      VABATB = SkSplitCSV(i)
093800030721     C                   ENDIF
093900030715     C*** RSD
094000160916     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
094100030715     C                   EVAL      VABRSD = SkSplitCSV(i)
094200030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
094300030715     C     '@':'A'       XLATE     VABRSD        VABRSD
094400030715     C* ==
094500030715     C                   ENDIF
094600030721     C*** RD2
094700160916     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
094800160902     C                   EVAL      VABRD2 = SkSplitCSV(i)
094900160902     C                   ENDIF
095000030715     C*** IND
095100160916     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
095200030715     C                   EVAL      VABIND = SkSplitCSV(i)
095300030715     C                   ENDIF
095400030715     C*** LOD
095500160916     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
095600160909     C                   EVAL      VABLOD = SkSplitCSV(i)
095700030715     C                   ENDIF
095800030715     C*** PRD
095900160916     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
096000030715     C                   EVAL      VABPRD = SkSplitCSV(i)
096100030715     C                   ENDIF
096200160902     C*** CAD
096300160916     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
096400160902     C                   EVAL      VABCAD = SkSplitCSV(i)
096500160902     C                   ENDIF
096600030721     C*** NZD
096700160916     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
096800030721     C                   EVAL      VABNZD = SkSplitCSV(i)
096900030721     C                   ENDIF
097000160916     C*** RMA
097100160916     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
097200160916     C                   EVAL      VABRMA = SkSplitCSV(i)
097300150515     C                   ENDIF
097400030715     C*** NOT
097500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
097600030715     C                   EVAL      VABNOT = SkSplitCSV(i)
097700030715     C                   ENDIF
097800030715     C*** NT2
097900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
098000030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
098100030715     C                   ENDIF
098200030715     C*** VCA
098300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
098400030715     C                   EVAL      VABVCA = SkSplitCSV(i)
098500030715     C                   ENDIF
098600030715     C*** VAS
098700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
098800030715     C                   EVAL      VABVAS = SkSplitCSV(i)
098900030715     C                   ENDIF
099000030715     C*** FFD
099100160902     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
099200030715     C                   EVAL      VABFFD = SkSplitCSV(i)
099300030715     C                   ENDIF
099400160902     C*** CBO
099500160902     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
099600160902     C                   EVAL      VABCBO = SkSplitCSV(i)
099700150420     C                   ENDIF
099800030721     C*** GC1
099900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
100000150420     C                   IF        %len(%trim(SkSplitCSV(i))) = 1
100100150420     C                   MOVEL     *blanks       wGC               1
100200150420     C                   EVAL      wGC = %trim(SkSplitCSV(i))
100300150420     C                   IF        SkSplitCSV(i) = 'M' OR
100400150420     C                             SkSplitCSV(i) = 'P'
100500150420     C                   MOVE(P)   wGC           VABGC1
100600150420     C                   ELSE
100700150420     C                   MOVEL(P)  wGC           VABGC1
100800150420     C                   ENDIF
100900150420     C                   ENDIF
101000030721     C                   ENDIF
101100030721     C*** GC2
101200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
101300120314     C                   IF        %len(%trim(SkSplitCSV(i))) = 1
101400120314     C                   MOVEL     *blanks       wGC               1
101500120314     C                   EVAL      wGC = %trim(SkSplitCSV(i))
101600120314     C                   IF        SkSplitCSV(i) = 'M' OR
101700120314     C                             SkSplitCSV(i) = 'P'
101800120314     C                   MOVE(P)   wGC           VABGC2
101900120314     C                   ELSE
102000120314     C                   MOVEL(P)  wGC           VABGC2
102100120314     C                   ENDIF
102200120314     C                   ENDIF
102300030721     C                   ENDIF
102400030721     C*** TSP
102500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
102600030721     C                   EVAL      VABTSP = SkSplitCSV(i)
102700030721     C                   ENDIF
102800030721     C*** NAS
102900160902     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
103000030721     C                   EVAL      VABNAS = SkSplitCSV(i)
103100030721     C                   ENDIF
103200030721     C*** TIC
103300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
103400030721     C                   EVAL      VABTIC = SkSplitCSV(i)
103500030721     C                   ENDIF
103600030721     C*** GCA
103700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
103800030721     C                   EVAL      VABGCA = SkSplitCSV(i)
103900030721     C                   ENDIF
104000030721     C*** XCO
104100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
104200030721     C                   EVAL      VABXCO = SkSplitCSV(i)
104300030721     C                   ENDIF
104400030721     C*** CTM
104500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
104600030721     C                   EVAL      VABCTM = SkSplitCSV(i)
104700030721     C                   ENDIF
104800030721     C*** TCR
104900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
105000030721     C                   EVAL      VABTCR = SkSplitCSV(i)
105100030721     C                   ENDIF
105200030721     C*** CTS
105300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
105400030721     C                   EVAL      VABCTS = SkSplitCSV(i)
105500030721     C                   ENDIF
105600030721     C*** FTM
105700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
105800030721     C                   EVAL      VABFTM = SkSplitCSV(i)
105900030721     C                   ENDIF
106000030721     C*** VAD
106100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
106200030721     C                   EVAL      VABVAD = SkSplitCSV(i)
106300030721     C                   ENDIF
106400030721     C*** GMA
106500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
106600030721     C                   EVAL      VABGMA = SkSplitCSV(i)
106700030721     C                   ENDIF
106800030721     C*** GGA
106900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
107000030721     C                   EVAL      VABGGA = SkSplitCSV(i)
107100030721     C                   ENDIF
107200030721     C*** GVA
107300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
107400030721     C                   EVAL      VABGVA = SkSplitCSV(i)
107500030721     C                   ENDIF
107600030721     C*** TC1
107700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
107800030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
107900030721     C                   ENDIF
108000030721     C*** TC2
108100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
108200030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
108300030721     C                   ENDIF
108400030721     C*** SCL
108500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
108600030721     C                   EVAL      VABSCL = SkSplitCSV(i)
108700030721     C                   ENDIF
108800030721     C*** RMO
108900160902     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
109000030721     C                   EVAL      VABRMO = SkSplitCSV(i)
109100030721     C                   ENDIF
109200160902     C*** CMO
109300160902     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
109400160902     C                   EVAL      VABCMO = SkSplitCSV(i)
109500160902     C                   ENDIF
109600030721     C*** NMO
109700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
109800030721     C                   EVAL      VABNMO = SkSplitCSV(i)
109900030721     C                   ENDIF
110000160908     C*** NOT_A
110100160916     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
110200160908     C                   EVAL      wVATNOT_A = SkSplitCSV(i)
110300160908     C                   ENDIF
110400160902     C*** NOT_B
110500160916     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
110600160902     C                   EVAL      wVATNOT_B = SkSplitCSV(i)
110700160902     C                   ENDIF
110800150420     C*** NOT_I
110900160916     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
111000150420     C                   EVAL      wVATNOT_I = SkSplitCSV(i)
111100150420     C                   ENDIF
111200150420     C*** NOT_S
111300160916     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_S'
111400150420     C                   EVAL      wVATNOT_S = SkSplitCSV(i)
111500150420     C                   ENDIF
111600160909     C*** NOT_E
111700160916     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
111800160909     C                   EVAL      wVATNOT_E = SkSplitCSV(i)
111900160909     C                   ENDIF
112000030721     C*
112100070705     C* Reperisco quindi i campi numerici...
112200030721     C*** RMN
112300160916     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
112400160916     C                   EVAL      wVATNOT_E = SkSplitCSV(i)                    *PERS
112500030721     C                   EVAL      PiStr=SkSplitCSV(i)
112600030721     C                   EXSR      CHKNUM
112700030721     C                   IF        PiInt=*on
112800030721     C                   Z-ADD     PiVal         VABRMN
112900160909     C                   Z-ADD     PiVal         VABNSP
113000030721     C                   ELSE
113100030721     C                   SETON                                        32
113200030721     C                   EVAL      VABRMN = 1
113300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113400160909     C                             + ' ' + 'VABRMN VABNSP'
113500030721     C                   ENDIF
113600030721     C                   ENDIF
113700030721     C*** NSP
113800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
113900030721     C                   EVAL      PiStr=SkSplitCSV(i)
114000030721     C                   EXSR      CHKNUM
114100100517     C                   IF        PiInt=*on
114200030721     C                   Z-ADD     PiVal         VABNSP
114300030721     C                   ELSE
114400030721     C                   SETON                                        32
114500030721     C                   EVAL      VABNSP = *zeros
114600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114700030721     C                             + ' ' + 'VABNSP'
114800030721     C                   ENDIF
114900030721     C                   ENDIF
115000160902     C*** NCL
115100160916     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
115200030721     C                   EVAL      PiStr=SkSplitCSV(i)
115300030721     C                   EXSR      CHKNUM
115400030721     C                   IF        PiInt=*on
115500030721     C                   Z-ADD     PiVal         VABNCL
115600030721     C                   ELSE
115700030721     C                   SETON                                        32
115800030721     C                   EVAL      VABNCL = *zeros
115900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116000030721     C                             + ' ' + 'VABNCL'
116100030721     C                   ENDIF
116200030721     C                   ENDIF
116300030721     C*** PKB
116400160916     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
116500030721     C                   EVAL      PiStr=SkSplitCSV(i)
116600030721     C                   EXSR      CHKNUM
116700030721     C                   IF        PiNum=*on
116800030721     C                   Z-ADD     PiVal         VABPKB
116900030721     C                   ELSE
117000030721     C                   SETON                                        32
117100030721     C                   EVAL      VABPKB = *zeros
117200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117300030721     C                             + ' ' + 'VABPKB'
117400030721     C                   ENDIF
117500030721     C                   ENDIF
117600160908     C*** CAS
117700160916     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
117800160915     C                   IF        SkSplitCSV(i) <> *blanks AND
117900160915     C                             SkSplitCSV(i) <> *zeros  AND
118000160915     C                             SkSplitCSV(i) <> '0,000'
118100160908     C                   EVAL      wFlgCAS = 1
118200160908     C                   ENDIF
118300160902     C                   EVAL      PiStr=SkSplitCSV(i)
118400160902     C                   EXSR      CHKNUM
118500160902     C                   IF        PiNum=*on
118600160902     C                   Z-ADD     PiVal         VABCAS
118700160902     C                   MOVEL     'EUR'         VABVCA
118800160902     C                   ELSE
118900160902     C                   SETON                                        32
119000160902     C                   EVAL      VABCAS = *zeros
119100160902     C                   EVAL      vinmsg = %trimr(vinmsg)
119200160902     C                             + ' ' + 'VABCAS'
119300160902     C                   ENDIF
119400160902     C                   ENDIF
119500030721     C*** CCM
119600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
119700030721     C                   EVAL      PiStr=SkSplitCSV(i)
119800030721     C                   EXSR      CHKNUM
119900030721     C                   IF        PiInt=*on      AND
120000030721     C                             PiVal<=9999999 AND
120100030721     C                             PiVal>*zeros
120200030721     C                   Z-ADD     PiVal         VABCCM
120300030721     C                   ELSE
120400030721     C                   SETON                                        32
120500030721     C                   EVAL      VABCCM = *zeros
120600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120700030721     C                             + ' ' + 'VABCCM'
120800030721     C                   ENDIF
120900030721     C                   ENDIF
121000030721     C*** LNP
121100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
121200030721     C                   EVAL      PiStr=SkSplitCSV(i)
121300030721     C                   EXSR      CHKNUM
121400030721     C                   IF        PiInt=*on    AND
121500030721     C                             PiVal<=999   AND
121600030721     C                             PiVal>*zeros
121700030721     C                   Z-ADD     PiVal         VABLNP
121800030721     C                   ELSE
121900030721     C                   SETON                                        32
122000030721     C                   EVAL      VABLNP = *zeros
122100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122200030721     C                             + ' ' + 'VABLNP'
122300030721     C                   ENDIF
122400030721     C                   ENDIF
122500030721     C*** NRS
122600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
122700030721     C                   EVAL      PiStr=SkSplitCSV(i)
122800030721     C                   EXSR      CHKNUM
122900030721     C                   IF        PiInt=*on AND
123000030721     C                             PiVal<=99
123100030721     C                   Z-ADD     PiVal         VABNRS
123200030721     C                   ELSE
123300030721     C                   SETON                                        32
123400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123500030721     C                             + ' ' + 'VABNRS'
123600030721     C                   ENDIF
123700030721     C                   ENDIF
123800030721     C*** LNA
123900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
124000120314     C                   IF        %trim(SkSplitCSV(i)) <> *zeros  AND
124100120314     C                             %trim(SkSplitCSV(i)) <> '0'     AND
124200120314     C                             %trim(SkSplitCSV(i)) <> '000'   AND
124300120314     C                             %trim(SkSplitCSV(i)) <> *blanks
124400030721     C                   EVAL      PiStr=SkSplitCSV(i)
124500030721     C                   EXSR      CHKNUM
124600030721     C                   IF        PiInt=*on    AND
124700030721     C                             PiVal<=999   AND
124800030721     C                             PiVal>*zeros
124900030721     C                   Z-ADD     PiVal         VABLNA
125000030721     C                   ELSE
125100030721     C                   SETON                                        32
125200030721     C                   EVAL      VABLNA = *zeros
125300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125400030721     C                             + ' ' + 'VABLNA'
125500120314     C                   ENDIF
125600030721     C                   ENDIF
125700030721     C                   ENDIF
125800030721     C*** CTR
125900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
126000030721     C                   EVAL      PiStr=SkSplitCSV(i)
126100030721     C                   EXSR      CHKNUM
126200030721     C                   IF        PiInt=*on  AND
126300030721     C                             PiVal<=999
126400030721     C                   Z-ADD     PiVal         VABCTR
126500030721     C                   ELSE
126600030721     C                   SETON                                        32
126700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126800030721     C                             + ' ' + 'VABCTR'
126900030721     C                   ENDIF
127000030721     C                   ENDIF
127100030721     C*** IAS
127200160902     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
127300030721     C                   EVAL      PiStr=SkSplitCSV(i)
127400030721     C                   EXSR      CHKNUM
127500030721     C                   IF        PiNum=*on
127600030721     C                   Z-ADD     PiVal         VABIAS
127700030721     C                   ELSE
127800030721     C                   SETON                                        32
127900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128000030721     C                             + ' ' + 'VABIAS'
128100030721     C                   ENDIF
128200030721     C                   ENDIF
128300030721     C*** VLB
128400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
128500030721     C                   EVAL      PiStr=SkSplitCSV(i)
128600030721     C                   EXSR      CHKNUM
128700030721     C                   IF        PiNum=*on
128800030721     C                   Z-ADD     PiVal         VABVLB
128900030721     C                   ELSE
129000030721     C                   SETON                                        32
129100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129200030721     C                             + ' ' + 'VABVLB'
129300030721     C                   ENDIF
129400030721     C                   ENDIF
129500030721     C*** QFT
129600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
129700030721     C                   EVAL      PiStr=SkSplitCSV(i)
129800030721     C                   EXSR      CHKNUM
129900030721     C                   IF        PiNum=*on
130000030721     C                   Z-ADD     PiVal         VABQFT
130100030721     C                   ELSE
130200030721     C                   SETON                                        32
130300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130400030721     C                             + ' ' + 'VABQFT'
130500030721     C                   ENDIF
130600030721     C                   ENDIF
130700030721     C*** NCD
130800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
130900030721     C                   EVAL      PiStr=SkSplitCSV(i)
131000030721     C                   EXSR      CHKNUM
131100030721     C                   IF        PiInt=*on      AND
131200030721     C                             PiVal<=9999999
131300030721     C                   Z-ADD     PiVal         VABNCD
131400030721     C                   ELSE
131500030721     C                   SETON                                        32
131600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131700030721     C                             + ' ' + 'VABNCD'
131800030721     C                   ENDIF
131900030721     C                   ENDIF
132000030721     C*** NCA
132100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
132200030721     C                   EVAL      PiStr=SkSplitCSV(i)
132300030721     C                   EXSR      CHKNUM
132400030721     C                   IF        PiInt=*on      AND
132500030721     C                             PiVal<=9999999
132600030721     C                   Z-ADD     PiVal         VABNCA
132700030721     C                   ELSE
132800030721     C                   SETON                                        32
132900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133000030721     C                             + ' ' + 'VABNCA'
133100030721     C                   ENDIF
133200030721     C                   ENDIF
133300030721     C*** ZNC
133400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
133500030721     C                   EVAL      PiStr=SkSplitCSV(i)
133600030721     C                   EXSR      CHKNUM
133700030721     C                   IF        PiInt=*on AND
133800030721     C                             PiVal<=99
133900030721     C                   Z-ADD     PiVal         VABZNC
134000030721     C                   ELSE
134100030721     C                   SETON                                        32
134200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134300030721     C                             + ' ' + 'VABZNC'
134400030721     C                   ENDIF
134500030721     C                   ENDIF
134600030721     C*** DCR
134700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
134800030721     C                   EVAL      PiStr=SkSplitCSV(i)
134900030721     C                   EXSR      CHKNUM
135000030721     C                   IF        PiInt=*on       AND
135100030721     C                             PiVal<=99999999
135200030721     C                   Z-ADD     PiVal         VABDCR
135300030721     C                   ELSE
135400030721     C                   SETON                                        32
135500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135600030721     C                             + ' ' + 'VABDCR'
135700030721     C                   ENDIF
135800030721     C                   ENDIF
135900030721     C*** HCR
136000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
136100030721     C                   EVAL      PiStr=SkSplitCSV(i)
136200030721     C                   EXSR      CHKNUM
136300030721     C                   IF        PiInt=*on   AND
136400030721     C                             PiVal<=9999
136500030721     C                   Z-ADD     PiVal         VABHCR
136600030721     C                   ELSE
136700030721     C                   SETON                                        32
136800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136900030721     C                             + ' ' + 'VABHCR'
137000030721     C                   ENDIF
137100030721     C                   ENDIF
137200030721     C*** VMD
137300160902     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
137400030721     C                   EVAL      PiStr=SkSplitCSV(i)
137500030721     C                   EXSR      CHKNUM
137600030721     C                   IF        PiNum=*on
137700030721     C                   Z-ADD     PiVal         VABVMD
137800030721     C                   ELSE
137900030721     C                   SETON                                        32
138000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138100030721     C                             + ' ' + 'VABVMD'
138200030721     C                   ENDIF
138300030721     C                   ENDIF
138400030721     C*** ANT
138500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
138600030721     C                   EVAL      PiStr=SkSplitCSV(i)
138700030721     C                   EXSR      CHKNUM
138800030721     C                   IF        PiInt=*on        AND
138900030721     C                             PiVal<=999999999
139000030721     C                   Z-ADD     PiVal         VABANT
139100030721     C                   ELSE
139200030721     C                   SETON                                        32
139300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
139400030721     C                             + ' ' + 'VABANT'
139500030721     C                   ENDIF
139600030721     C                   ENDIF
139700030715     C*
139800030715     C                   ENDIF
139900030715     C                   EVAL      i = i + 1
140000030715     C                   ENDDO
140100070705     C*
140200160902     C* Considerazioni particolari "fuori ciclo"
140300110706     C*
140400160908     C* Verifica campi "particolari" VAB
140500160908     C                   EXSR      CHKVAB
140600160908     C*
140700030715     C                   ENDIF
140800020204     C*
140900010607     C                   ENDSR
141000010607     C*----------------------------------------------------*
141100160908
141200160908
141300160908     C*----------------------------------------------------*
141400160908     C*  CONSIDERAZIONI SU CAMPI PARTICOLARI VAB
141500160908     C*----------------------------------------------------*
141600160908     C     CHKVAB        BEGSR
141700160908     C*
141800160908     C*** RMN
141900160908     C                   IF        *in71
142000160908     C                   EVAL      VABRMN = VABNSP
142100160908     C                   ENDIF
142200160908     C                   IF        VABRMN = *zeros
142300160908     C                   EVAL      VABRMN = 1
142400160908     C                   ENDIF
142500160908     C*** NZD
142600160908     C                   IF        VABNZD = 'IT'  OR
142700160908     C                             VABNZD = 'ITA' OR
142800160908     C                             VABNZD = 'Ita' OR
142900160908     C                             VABNZD = 'ita'
143000160908     C                   EVAL      VABNZD = *blanks
143100160908     C                   ENDIF
143200160908     C*** CTR
143300160908     C                   IF        *in72
143400160908     C                   IF        VABNZD <> *blanks
143500160908     C                   EVAL      VABCTR =  wVABCTR_E
143600160908     C                   ENDIF
143700160908     C                   ENDIF
143800160908     C*** CBO
143900160908     C                   IF        VABCBO = *blanks
144000160908     C                   EVAL      VABCBO = '1'
144100160908     C                   ENDIF
144200160908     C*** NCL
144300160908     C                   IF        VABNCL = *zeros
144400160908     C                   EVAL      VABNCL = 1
144500160908     C                   ENDIF
144600160908     C*** CCM
144700160908     C                   IF        VABCCM = *zeros
144800160908     C                   EVAL      PiStr=VLRKSC
144900160908     C                   EXSR      CHKNUM
145000160908     C                   IF        PiInt=*on
145100160908     C                   Z-ADD     PiVal         VABCCM
145200160908     C                   ELSE
145300160908     C                   SETON                                        31
145400160908     C                   ENDIF
145500160908     C                   ENDIF
145600160908     C*** LNP
145700160908     C                   IF        VABLNP = *zeros
145800160908     C                   IF        VLRPOI = 046 OR
145900160908     C                             VLRPOI = 789 OR
146000160908     C                             VLRPOI = 999
146100160908     C                   MOVEL     VABCCM        VABLNP
146200160908     C                   ELSE
146300160908     C                   EVAL      VABLNP = VLRPOI
146400160908     C                   ENDIF
146500160908     C                   ENDIF
146600160908     C*** LNP
146700160908     C                   IF        VABFGS = *zeros
146800160908     C                   EVAL      VABFGS = VABLNP
146900160908     C                   ENDIF
147000160908     C*** FGS / LNP
147100160908     C                   IF        VABFGS = *zeros OR
147200160908     C                             VABLNP = *zeros
147300160908     C                   SETON                                        31
147400160908     C                   ENDIF
147500160908     C*** CAS
147600160908     C                   IF        wFlgCAS = *zeros
147700160908     C                   EVAL      VABCAS = *zeros
147800160908     C                   EVAL      VABVCA = *blanks
147900160908     C                   ENDIF
148000160908     C*** NOT
148100160908     C                   IF        VABNOT = *blanks AND wVABNOT <> *blanks
148200160908     C                   EVAL      VABNOT = wVABNOT
148300160908     C                   ENDIF
148400160908     C*
148500160908     C* Se provincia nn valorizzata la reperisco
148600160908     C* tramite TISI95R a seconda dei dati d instradamento presenti
148700160908     C                   IF        VABPRD = *blanks
148800160908     C                   CLEAR                   TISI95DS
148900160908     C                   EVAL      I95TCN = '3'
149000160908     C                   Z-ADD     datcor        I95DAT
149100160908     C                   EVAL      I95NAR = VABNZD
149200160908     C                   EVAL      I95CAP = VABCAD
149300160908     C                   EVAL      I95LOC = VABLOD
149400160908     C                   CALL      'TISI95R'
149500160908     C                   PARM                    TISI95DS
149600160908     C                   EVAL      VABPRD = O95PRV
149700160908     C                   ENDIF
149800160908     C*
149900160908     C* Eseguo routine finale x considerazioni specifiche su importi/divise
150000160908     C                   EXSR      CHKIMPDIV
150100160908     C*
150200160908     C                   ENDSR
150300160908     C***
150400010601
150500020204
150600020204     C*----------------------------------------------------*
150700020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
150800020204     C*----------------------------------------------------*
150900020204     C     CHKIMPDIV     BEGSR
151000060524     C*
151100060524     C* Verifico il codice bolla in base al tipo porto e alla presenza o meno del contrassegno
151200160906     C                   IF        vabCAS  > *zeros OR
151300160906     C                             wFlgCAS > *zeros
151400060524     C                   IF        vabCBO = '1'
151500060524     C                   EVAL      vabCBO = '4'
151600060524     C                   ENDIF
151700060524     C                   IF        vabCBO = '2'
151800060524     C                   EVAL      vabCBO = '6'
151900060524     C                   ENDIF
152000060524     C                   ENDIF
152100020204     C*
152200020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
152300020204     C                   Z-ADD     *zeros        wrkDec            9 9
152400020204     C*
152500020204     C* Come prima cosa effettuo considerazioni sulla divisa
152600020204     C                   IF        vabIAS > *zeros
152700150420     C                   IF        vabVAS =  *blanks
152800150420     C                   EVAL      vabVAS =  'EUR'
152900020204     C                   ENDIF
153000020204     C                   ENDIF
153100020204     C*
153200020204     C                   IF        vabCAS > *zeros
153300150420     C                   IF        vabVCA <> *blanks
153400150420     C                   EVAL      vabVCA =  'EUR'
153500020204     C                   ENDIF
153600020204     C                   ENDIF
153700020204     C*
153800020204     C                   IF        vabVMD > *zeros
153900150420     C                   IF        vabVAD <> *blanks
154000150420     C                   EVAL      vabVAD =  'EUR'
154100020204     C                   ENDIF
154200020204     C                   ENDIF
154300020204     C*
154400020204     C                   ENDSR
154500020204     C***
154600010330
154700010330
154800010330     C*----------------------------------------------------*
154900010330     C*  CONTROLLO NUMERICITA' CAMPI
155000010330     C*----------------------------------------------------*
155100010330     C     CHKNUM        BEGSR
155200010330     C*
155300010606     C                   IF        PiDecChr = *blanks
155400030715     C                   EVAL      PiDecChr = CharNUM
155500010606     C                   ENDIF
155600010606     C*
155700010606     C                   CALL(e)   'ISNUMERIC'
155800010330     C                   PARM                    PiStr            30
155900010606     C                   PARM                    PiDecChr          1
156000010330     C                   PARM      *ZEROS        PiVal            30 9
156100010330     C                   PARM      '0'           PiInt             1
156200010330     C                   PARM      '0'           PiNum             1
156300010330     C                   IF        %error
156400010606     C                   EVAL      PiNum=*off
156500010330     C                   ENDIF
156600010330     C*
156700010330     C                   ENDSR
156800010330     C***
156900160906
157000160906
157100160906      /TITLE Valorizzazione Progressivo Applicazione
157200160906     C     calprog       begsr
157300160906     C*
157400160906     C                   movel     *blanks       dwlisv            2
157500160906     C                   movel     *all'0'       dwlprg           10
157600160906     C                   z-add     *zeros        wrkprg            8 0
157700160906     C*
157800160906     C                   eval      dwlisv = vlrisv
157900160906     C*
158000170324     C     *start        setll     tis7prgf
158100160906     C                   read(e)   tis7prgf
158200160906     C                   if        not %error
158300160906     C                   eval      dwlprg = f_tis7prgf
158400160906     C*
158500160906     C                   move(p)   dwlprg        wrkprg
158600160906     C                   add       1             wrkprg
158700160906     C                   move(p)   wrkprg        dwlprg
158800160906     C                   movel     dwlisv        dwlprg
158900160906     C*
159000160906     C                   eval      f_tis7prgf = dwlprg
159100160906     C                   update    tis7prg0
159200170324     C                   else
159300170324     C                   eval      f_tis7prgf = 'XX00000000'
159400160906     C                   endif
159500160906     C*
159600160906     C                   endsr
159700050421
159800160908
159900160908     C     *pssr         BEGSR
160000160908     C*
160100160908     C                   if        %open(tivin00r)
160200160908     C                   close     tivin00r
160300160908     C                   endif
160400160908     C                   if        %open(fivabwwr)
160500160908     C                   close     fivabwwr
160600160908     C                   endif
160700160908     C                   if        %open(fivatwwr)
160800160908     C                   close     fivatwwr
160900160908     C                   endif
161000160908     C*
161100160908     C* Effettuo la chiamata al CLLE preposto
161200160908     C  N50              call(e)   'TITVVTC'
161300160908     C                   parm                    parccm
161400160908     C                   parm                    parmbr
161500160908     C                   parm      '2'           paropz
161600160908     C*
161700160908     C                   eval      wrkesito = '2'
161800160908     C*
161900160908     C                   seton                                        LR
162000160908     C*
162100160908     C                   ENDSR     '*CANCL'
162200160908     C***
162300100419
162400100419
162500160902     C     normCSV       BEGSR
162600160902     C*
162700160902     C                   z-add     *zeros        wLenVINDTA        4 0
162800160902     C                   z-add     *zeros        wFlgVINDTA        1 0
162900160902     C                   setoff                                       80
163000160902     C*
163100160902     C                   dow       wLenVINDTA < %len(vindta)
163200160902     C                   eval      wLenVINDTA = wLenVINDTA + 1
163300160902     C                   movel     *blanks       wAlfa1            1
163400160902     C                   eval      wAlfa1 = %subst(vindta:wLenVINDTA:1)
163500160902     C*
163600160902     C* Se byte inferiore a HEX40 è un byte "non stampabile" per cui considero *blank
163700160902     C                   if        wAlfa1 < X'40'
163800160902     C                   eval      %subst(vindta:wLenVINDTA:1) = *blanks
163900160902     C                   endif
164000160902     C*
164100160902     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
164200160902     C                   if        wAlfa1 <> CharCSV and
164300160902     C                             wAlfa1 <> CharTXT and
164400160902     C                             wAlfa1 <> CharNUM
164500160902     C                   z-add     1             wFlgVINDTA
164600160902     C                   endif
164700160902     C*
164800160902     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
164900160902     C* separatori campi
165000160902     C                   if        CharTXT <> *blanks
165100160902     C                   if        wAlfa1 = CharTXT
165200160902     C                   if        not *in80
165300160902     C                   seton                                        80
165400160902     C                   else
165500160902     C                   setoff                                       80
165600160902     C                   endif
165700160902     C                   endif
165800160902     C                   if        wAlfa1 = CharCSV and *in80 = *on
165900160902     C                   eval      %subst(vindta:wLenVINDTA:1) = CharSOS
166000160902     C                   endif
166100160902     C                   endif
166200160902     C*
166300160902     C                   enddo
166400100419     C*
166500100419     C                   ENDSR
166600100419     C***
166700150420
166800150420
166900150420      /TITLE Scrittura record in file TIVGD00F (file VAS generico download)
167000160902     C     wriVAB_VGD    BEGSR
167100160909     C*
167200160909     C* Verifica campi "particolari" VAB
167300160909     C                   exsr      repNSP
167400160909     C*
167500160909     C                   setoff                                       45
167600160909     C                   if        depSPE  = *zeros OR
167700160909     C                             depSPE <> vabNSP
167800160909     C                   seton                                        45
167900160909     C                   eval      depSPE  = vabNSP
168000160909     C                   endif
168100160909     C*
168200160909     C                   if        not *in31
168300150420     C*
168400160902     C* Se primo giro allora scrivo testata
168500160902     C                   if        wVGD_Head = *zeros
168600160902     C                   eval      wVGD_Head = 1
168700160902     C                   eval      wBufferOut =
168800160902     C                             %trim(pptCHR)+'VABCCM' +%trim(pptCHR)+pptCSV+
168900160902     C                             %trim(pptCHR)+'VABLNP' +%trim(pptCHR)+pptCSV+
169000160902     C                             %trim(pptCHR)+'VABCTR' +%trim(pptCHR)+pptCSV+
169100160902     C                             %trim(pptCHR)+'VABRSD' +%trim(pptCHR)+pptCSV+
169200160902     C                             %trim(pptCHR)+'VABRD2' +%trim(pptCHR)+pptCSV+
169300160902     C                             %trim(pptCHR)+'VABIND' +%trim(pptCHR)+pptCSV+
169400160902     C                             %trim(pptCHR)+'VABLOD' +%trim(pptCHR)+pptCSV+
169500160902     C                             %trim(pptCHR)+'VABCAD' +%trim(pptCHR)+pptCSV+
169600160902     C                             %trim(pptCHR)+'VABPRD' +%trim(pptCHR)+pptCSV+
169700160902     C                             %trim(pptCHR)+'VABCBO' +%trim(pptCHR)+pptCSV+
169800160902     C                             %trim(pptCHR)+'VABRMO' +%trim(pptCHR)+pptCSV+
169900160902     C                             %trim(pptCHR)+'VABRMN' +%trim(pptCHR)+pptCSV+
170000160902     C                             %trim(pptCHR)+'VABRMA' +%trim(pptCHR)+pptCSV+
170100160902     C                             %trim(pptCHR)+'VABNAS' +%trim(pptCHR)+pptCSV+
170200160902     C                             %trim(pptCHR)+'VABNCL' +%trim(pptCHR)+pptCSV+
170300160902     C                             %trim(pptCHR)+'VABPKB' +%trim(pptCHR)+pptCSV+
170400160902     C                             %trim(pptCHR)+'VABCAS' +%trim(pptCHR)+pptCSV+
170500160902     C                             %trim(pptCHR)+'VABVCA' +%trim(pptCHR)+pptCSV+
170600160902     C                             %trim(pptCHR)+'VABFFD' +%trim(pptCHR)+pptCSV+
170700160902     C                             %trim(pptCHR)+'VABIAS' +%trim(pptCHR)+pptCSV+
170800160902     C                             %trim(pptCHR)+'VABVAS' +%trim(pptCHR)+pptCSV+
170900160902     C                             %trim(pptCHR)+'VABNOT' +%trim(pptCHR)+pptCSV+
171000160902     C                             %trim(pptCHR)+'VABNRC' +%trim(pptCHR)+pptCSV+
171100160902     C                             %trim(pptCHR)+'VABTRC' +%trim(pptCHR)+pptCSV+
171200160902     C                             %trim(pptCHR)+'VABEMD' +%trim(pptCHR)+pptCSV+
171300160902     C                             %trim(pptCHR)+'VABCEL' +%trim(pptCHR)
171400160902     C                   exsr      wriVGD
171500160902     C                   endif
171600160902     C*
171700160902     C* Quindi scrivo dati spedizione
171800160909     C   45              eval      wBufferOut =
171900150420     C                             %trim(pptCHR)+%trim(%editc(VABCCM:'X'))
172000150420     C                                                    +%trim(pptCHR)+pptCSV+
172100150420     C                             %trim(pptCHR)+%trim(%editc(VABLNP:'X'))
172200150420     C                                                    +%trim(pptCHR)+pptCSV+
172300150420     C                             %trim(pptCHR)+%trim(%editc(VABCTR:'X'))
172400150420     C                                                    +%trim(pptCHR)+pptCSV+
172500150420     C                             %trim(pptCHR)+%trim(VABRSD)
172600150420     C                                                    +%trim(pptCHR)+pptCSV+
172700160902     C                             %trim(pptCHR)+%trim(VABRD2)
172800160902     C                                                    +%trim(pptCHR)+pptCSV+
172900150420     C                             %trim(pptCHR)+%trim(VABIND)
173000150420     C                                                    +%trim(pptCHR)+pptCSV+
173100150420     C                             %trim(pptCHR)+%trim(VABLOD)
173200150420     C                                                    +%trim(pptCHR)+pptCSV+
173300150420     C                             %trim(pptCHR)+%trim(VABCAD)
173400150420     C                                                    +%trim(pptCHR)+pptCSV+
173500150420     C                             %trim(pptCHR)+%trim(VABPRD)
173600150420     C                                                    +%trim(pptCHR)+pptCSV+
173700150420     C                             %trim(pptCHR)+%trim(VABCBO)
173800150420     C                                                    +%trim(pptCHR)+pptCSV+
173900150420     C                             %trim(pptCHR)+%trim(VABRMO)
174000150420     C                                                    +%trim(pptCHR)+pptCSV+
174100150420     C                             %trim(pptCHR)+%trim(%editc(VABRMN:'4'))
174200150420     C                                                    +%trim(pptCHR)+pptCSV+
174300150420     C                             %trim(pptCHR)+%trim(VABRMA)
174400150420     C                                                    +%trim(pptCHR)+pptCSV+
174500150420     C                             %trim(pptCHR)+%trim(VABNAS)
174600150420     C                                                    +%trim(pptCHR)+pptCSV+
174700150420     C                             %trim(pptCHR)+%trim(%editc(VABNCL:'4'))
174800150420     C                                                    +%trim(pptCHR)+pptCSV+
174900150420     C                             %trim(pptCHR)+%trim(%editc(VABPKB:'4'))
175000150420     C                                                    +%trim(pptCHR)+pptCSV+
175100150420     C                             %trim(pptCHR)+%trim(%editc(VABCAS:'4'))
175200150420     C                                                    +%trim(pptCHR)+pptCSV+
175300150420     C                             %trim(pptCHR)+%trim(VABVCA)
175400150420     C                                                    +%trim(pptCHR)+pptCSV+
175500150420     C                             %trim(pptCHR)+%trim(VABFFD)
175600150420     C                                                    +%trim(pptCHR)+pptCSV+
175700150420     C                             %trim(pptCHR)+%trim(%editc(VABIAS:'4'))
175800150420     C                                                    +%trim(pptCHR)+pptCSV+
175900150420     C                             %trim(pptCHR)+%trim(VABVAS)
176000150420     C                                                    +%trim(pptCHR)+pptCSV+
176100150421     C                             %trim(pptCHR)+%trim(VABNOT)
176200150421     C                                                    +%trim(pptCHR)+pptCSV+
176300160902     C                             %trim(pptCHR)+%trim(wVATNOT_A)
176400160902     C                                                    +%trim(pptCHR)+pptCSV+
176500160902     C                             %trim(pptCHR)+%trim(wVATNOT_B)
176600160902     C                                                    +%trim(pptCHR)+pptCSV+
176700150420     C                             %trim(pptCHR)+%trim(wVATNOT_I)
176800150420     C                                                    +%trim(pptCHR)+pptCSV+
176900150420     C                             %trim(pptCHR)+%trim(wVATNOT_S)
177000150420     C                                                    +%trim(pptCHR)
177100160909     C   45              exsr      wriVGD
177200160909     C*
177300160909     C                   endif
177400150420     C*
177500150420     C                   ENDSR
177600150420     C*------------------------------------------------------------------------*
177700150420
177800150420
177900150420
178000150420      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
178100150420     C     wriVGD        BEGSR
178200150420     C*
178300150420     C                   clear                   tivgd000
178400150420     C                   eval      vgdDTA = wBufferOut
178500150420     C                   eval      vgdTIP = 'EB'
178600150420     C                   eval      vgdKSU = vlrKSC
178700150420     C                   eval      vgdTSC = 'WW'
178800150420     C                   eval      vgdDAT = datcor
178900160906     C                   eval      vgdPRG = f_tis7prgf
179000160920     C                   eval      vgdPGM = procname
179100160906     C                   eval      vgdSTO = '?'
179200150420     C                   write     tivgd000
179300150420     C*
179400150420     C                   ENDSR
179500150420     C*------------------------------------------------------------------------*
179600160902
179700160902
179800160902     C*----------------------------------------------------*
179900160902     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
180000160902     C*----------------------------------------------------*
180100160902     C     SR_SPLIT      BEGSR
180200160902     C*
180300160902     C* Chiamo il *pgm d utilità x lo "split" della riga delle intestazioni
180400160902     C                   CALL      'XSPLIT2'
180500160902     C                   PARM                    InStringa
180600160902     C                   PARM                    InSepara
180700160902     C                   PARM      '5'           InTypeOut
180800160902     C                   PARM      ''            wSkParam
180900160902     C                   PARM                    OutErrore
181000160902     C                   PARM                    OutMsg
181100160902     C                   MOVEA     wSkParam      SkSplitCSV_5
181200160902     C*
181300160902     C                   ENDSR
181400150420
181500150420
181600000613     C     *inzsr        BEGSR
181700990910     C*
181800990910     C     *entry        plist
181900990920     C                   parm                    tivlrds
182000990921     C                   parm      wrkesito      esito
182100000724     C                   parm                    prmlit
182200000710     C                   parm                    prmfir
182300010330     C*
182400010330     C* CALCOLA LA DATA CORRENTE
182500150420     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
182600150420     C                   eval      datcor = %dec(%date() : *ISO)
182700000613     C*
182800000613     C                   ENDSR
182900000613     C***
