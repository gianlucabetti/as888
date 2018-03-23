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
003200160902     D wVABRMA_1       s             15    inz
003300160902     D wVABRMA_2       s             15    inz
003400160902     D wVABNOT         s             35    inz
003500160906     D wVABCTR_E       s                   like(VABCTR) inz
003600150420     D wrkesito        s                   like(esito)
003700160902     D wVGD_Head       s              1  0 inz(*zeros)
003800160902     D wFlgCAS         s              1  0 inz(*zeros)
003900160902     D s_vindta        s                   like(vindta) inz(*blanks)
004000160908     D parccm          s              8    inz(*blanks)
004100160908     D parmbr          s             10    inz(*blanks)
004200160908     D paropz          s              1    inz(*blanks)
004300160908     D chkcall         s              1    inz(*blanks)
004400160909     D depSPE          s              7  0 inz
004500160902
004600160908
004700160908     D*------------------
004800160908     D* DS REPERIMENTO NUMERATORE
004900160908     D*------------------
005000160908     D trul33ds      e ds                  inz
005100160908     D*------------------
005200160908     D* DS ARCHITETTURA
005300160908     D*------------------
005400160908     D KPJBA         e ds                  inz
005500160908
005600160902
005700160902     D*------------------
005800160902     D* VARIABILI X LA CHIAMATA AL *PGM UTILITA' "XSPLIT"
005900160902     D*------------------
006000160902     D InStringa       S          65535A   VARYING                              (stringa input)
006100160902     D InSepara        S             10A                                        (separatore)
006200160902     D InTypeOut       S              1A                                        (tipo output)
006300160902     D wSkParam        S          65535A                                        (output)
006400160902     D OutErrore       S              1A                                        (flag errore)
006500160902     D OutMsg          S             80A                                        (messaggio errore)
006600160902     D SkSplitCSV_5    S            256    DIM(256)
006700160902
006800160902
006900030715     D*------------------
007000030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
007100030715     D*------------------
007200160902     D SkSplitFLD      S                   LIKE(SkSplitCSV_5) DIM(256)
007300160902     D SkSplitCSV      S                   LIKE(SkSplitCSV_5) DIM(256)
007400160902     D CharCSV         S              1
007500160902     D CharTXT         S              1
007600160902     D CharNUM         S              1
007700160902     D CharSOS         S              1
007800160902     D i               s              3  0 INZ(1)
007900030715     D wGiro           s              1  0 INZ(*zeros)
008000030822     D*------------------
008100030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
008200030822     D*------------------
008300160902     D posDaDft        S              3  0 INZ(*zeros)
008400160902     D posADft         S              3  0 INZ(*zeros)
008500160902     D wGiroDft        s              1  0 INZ(*zeros)
008600160908
008700160908
008800031201     D*------------------
008900031201     D* Costanti
009000031201     D*------------------
009100160902     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
009200160908     D MAIU            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
009300150420
009400010330
009500010330
009600990921     C                   reset                   esito
009700990921     C                   reset                   wrkesito
009800010601     C*
009900160908     C                   exsr      defcam
010000160908     C   50              exsr      calprog
010100150420     C                   exsr      rwfile
010200160908     C   50              exsr      strvgd
010300160906     C                   exsr      endela
010400160906     C*
010500010601     C                   seton                                        lr
010600160906
010700160906
010800160906     C*--------------------------------------------------------
010900160906     C* STRVGD - "LIBERO" LA TRANSAZIONE (GRUPPO) CORRENTE    *
011000160906     C*--------------------------------------------------------
011100160906     C     STRVGD        BEGSR
011200160906     C*
011300160906     C
011400170119     C/EXEC SQL
011500170227     C+ UPDATE TIVGD00F SET VGDSTO = 'R'
011600170119     C+ WHERE VGDSTO = '?'     AND VGDPRG = :VGDPRG AND
011700170119     C+       VGDTIP = :VGDTIP AND VGDKSU = :VGDKSU AND
011800170119     C+       VGDTSC = :VGDTSC
011900170119     C/END-EXEC
012000160906     C
012100160906     C*
012200160906     C                   ENDSR
012300160906     C***
012400160902
012500010601
012600010601     C*--------------------------------------------------------
012700160902     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
012800010601     C*--------------------------------------------------------
012900010601     C     ENDELA        BEGSR
013000080222     C*
013100080222     C* Effettuo lancio TISI95 solo x chiusura
013200080222     C                   CLEAR                   TISI95DS
013300080222     C                   EVAL      I95TLA = 'C'
013400080222     C                   CALL      'TISI95R'
013500080222     C                   PARM                    TISI95DS
013600160908     C*
013700160908     C* Effettuo la chiamata al CLLE preposto
013800160908     C  N50              call(e)   'TITVVTC'
013900160908     C                   parm                    parccm
014000160908     C                   parm                    parmbr
014100160908     C                   parm      '2'           paropz
014200000613     C*
014300010601     C                   ENDSR
014400010601     C***
014500010601
014600010601
014700010330     C*--------------------------------------------------------
014800150420     C* RWFILE  LEGGE TIVIN00R E SCRIVE FIVABWWF              *
014900010330     C*--------------------------------------------------------
015000150420     C     RWFILE        BEGSR
015100010330     C*
015200010330     C                   if        not %open(tivin00r)
015300010330     C                   open      tivin00r
015400010330     C                   endif
015500160908     C*
015600160908     C* Eseguo operazioni di aggiunta nuovo membro file FNVAT
015700160908     C  N50              exsr      prevat
015800160908     C   50              eval      chkcall = '0'
015900160908     C*
016000160908     C                   if        chkcall = '0'
016100160908     C*
016200160908     C                   if        not %open(fivabwwr)
016300160908     C                   open      fivabwwr
016400160908     C                   endif
016500160908     C*
016600160908     C                   if        not %open(fivatwwr)
016700160908     C                   open      fivatwwr
016800160908     C                   endif
016900010330     C*
017000160908     C                   clear                   §CTROKVB          7 0
017100160908     C                   clear                   §CTROKVT          7 0
017200160908     C                   clear                   §CTRMO            7 0
017300160908     C                   clear                   §CTRNO            7 0
017400010330     C*
017500030822     C                   do        *HIVAL
017600010330     C*
017700030822     C                   read      tivin00r                               70
017800160902     C*
017900160902     C* Salvo il buffer orginale ricevuto dal cliente
018000160902     C                   eval      s_vindta = vindta
018100160902     C*
018200160902     C                   if        *in70 = *off and
018300160902     C                             vindta <> *blanks and
018400160902     C                             (vinflg = *blanks
018500160902     C                              or vinflg = '0'
018600160902     C                              or vinflg = '2')
018700160902     C*
018800160902     C* Controlli di "base" sui dati grezzi
018900160902     C                   exsr      normCSV
019000040510     C*
019100160902     C                   if        wFlgVINDTA = 1
019200010330     C*
019300010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
019400010711     C                   if        vinflg = *blanks or vinflg = '0'
019500010711     C                   clear                   vinmsg
019600010711     C                   endif
019700160902     C*
019800160902 xxx C* Porto eventualmente da minuscolo in maiuscolo le INTESTAZIONI colonne cliente
019900160908 xxx C***  minu:MAIU     XLATE     vindta        vindta
020000160902     C*
020100160902     C* Splitto i dati
020200160902     C                   CLEAR                   SkSplitCSV_5
020300160902     C                   EVAL      InStringa = vindta
020400160902     C                   EVAL      InSepara  = CharCSV
020500160902     C                   EXSR      SR_SPLIT
020600160902     C*
020700160902     C*****************************
020800160909 xxx C* A questo punto verifico che le colonne mandatorie siano valorizzate correttamente
020900160909     C                   if        %len(%trim(SkSplitCSV_5(2)))  > 0 and
021000160909     C                             %len(%trim(SkSplitCSV_5(7)))  > 0 and
021100160909     C                             %len(%trim(SkSplitCSV_5(13))) > 0
021200160902     C*****************************
021300010601     C*
021400160902     C* Se OK => procedo con elaborazione
021500010330     C                   exsr      inzvar
021600010605     C                   exsr      impvab
021700010601     C*
021800010604     C* Ebbene...
021900160908     C                   if        wGiro > 1
022000160908     C  N31              add       1             §CTROKVB
022100160908     C   32              add       1             §CTRMO
022200160908     C   31              add       1             §CTRNO
022300160908     C*
022400160902     C   50              exsr      wriVAB_VGD
022500160908     C  N50              exsr      wriVAB_WW
022600150420     C                   endif
022700010604     C*
022800010604     C                   if        *in31 = *off and
022900010604     C                             *in32 = *off
023000010604     C                   eval      vinflg = '1'
023100010604     C                   else
023200010604     C                   eval      vinflg = '2'
023300010604     C                   endif
023400010601     C*
023500160902     C                   else
023600160902     C                   eval      vinflg = '1'
023700010604     C                   endif
023800160902     C*
023900160902     C                   else
024000160902     C                   eval      vinflg = '1'
024100160902     C                   endif
024200010604     C*
024300010330     C                   else
024400010330     C                   eval      vinflg = '1'
024500010330     C                   endif
024600010601     C*
024700160902     C                   eval      vindta = s_vindta
024800010601     C  N70              update    tivin000
024900010330     C*
025000030822     C  N70              enddo
025100010601     C
025200010601     C*
025300160902     C* Stabilisco l'esito della traduzione
025400160902     C                   select
025500160902     C*
025600160902     C* Se non ci sono record con errori => stato OK
025700160902     C                   when      §ctrno = 0 and
025800160902     C                             §ctrmo = 0
025900010330     C                   eval      wrkesito = '0'
026000160902     C*
026100160902     C* Se nessun record OK => errore totale
026200160908     C                   when      §ctrokvb = 0
026300160902     C                   eval      wrkesito = '2'
026400160902     C*
026500160902     C* Diversamente => errore parziale/warning
026600160902     C                   other
026700160902     C                   eval      wrkesito = '1'
026800160902     C*
026900160902     C                   endsl
027000160908     C*
027100160908     C                   endif
027200160908     C
027300160908     C                   if        chkcall = '1'
027400160908     C                   eval      wrkesito = '2'
027500160908     C                   endif
027600010330     C*
027700010330     C                   if        %open(tivin00r)
027800010330     C                   close     tivin00r
027900010330     C                   endif
028000160908     C*
028100160908     C                   if        %open(fivabwwr)
028200160908     C                   close     fivabwwr
028300160908     C                   endif
028400160908     C                   if        %open(fivatwwr)
028500160908     C                   close     fivatwwr
028600160908     C                   endif
028700160908     C*
028800160908     C                   if        not *in50        and
028900160908     C                             wrkesito <> '2'  and
029000160908     C                             vlrpoi <> *zeros
029100160908     C                   exsr      invio
029200160908     C                   endif
029300010330     C*
029400010330     C                   ENDSR
029500010330     C***
029600160908
029700160908
029800160908     C*----------------------------------------------------*
029900160908     C*  SCARICAMENTO BUFFER RECORDS VAB IN "WW"
030000160908     C*----------------------------------------------------*
030100160908     C     WRIVAB_WW     BEGSR
030200160908     C*
030300160908     C* Verifica campi "particolari" VAB
030400160908     C                   exsr      repNSP
030500160908     C*
030600160909     C                   setoff                                       45
030700160909     C                   if        depSPE  = *zeros OR
030800160909     C                             depSPE <> vabNSP
030900160909     C                   seton                                        45
031000160909     C                   eval      depSPE  = vabNSP
031100160909     C                   endif
031200160909     C*
031300160909     C                   if        not *in31
031400160909     C                   exsr      EXEVATE
031500160909     C   45              exsr      EXEVATA
031600160909     C   45              exsr      EXEVATB
031700160909     C   45              exsr      EXEVATI
031800160909     C   45              exsr      EXEVATJ
031900160909     C   45              exsr      EXEVATS
032000160909     C   45              write     fivab000                                     => scarico il VAB
032100160908     C                   endif
032200160908     C*
032300160908     C                   ENDSR
032400160908
032500160908
032600160908     C*----------------------------------------------------*
032700160908     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
032800160908     C*----------------------------------------------------*
032900160908     C     REPNSP        BEGSR
033000160908     C*
033100160909     C* NSP => Se necessario stacco un numeratore da AZNUM
033200160909     C                   if        VABNSP = *zeros
033300160908     C                   clear                   TRUL33DS
033400160908     C                   eval      I33OPE = *zeros
033500160908     C                   eval      I33CNU = 302
033600160908     C                   eval      I33NUM = 1
033700160908     C                   movel     TRUL33DS      KPJBU
033800160908     C                   call      'TRUL33R'
033900160908     C                   parm                    KPJBA
034000160908     C                   movel     KPJBU         TRUL33DS
034100160908     C                   if        O33ERR = *zeros
034200160908     C                   z-add     O33NRF        VABNSP
034300160908     C                   z-add     O33NRF        VATNSP
034400160908     C                   else
034500160908     C                   SETON                                        31
034600160908     C                   EVAL      vinmsg = %trimr(vinmsg)
034700160908     C                             + ' ' + 'VABNSP VATNSP'
034800160908     C                   endif
034900160909     C                   endif
035000160908     C*
035100160908     C                   ENDSR
035200160908
035300160908
035400160908     C*----------------------------------------------------*
035500160908     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
035600160908     C*----------------------------------------------------*
035700160908     C     EXEVATA       BEGSR
035800160908     C*
035900160908     C                   EVAL      VATTRC = 'A'
036000160908     C                   EVAL      VATNOT = wVATNOT_A
036100160908     C                   exsr      wrivat                                       => scarico VAT
036200160908     C*
036300160908     C                   ENDSR
036400160908     C*----------------------------------------------------*
036500160908     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
036600160908     C*----------------------------------------------------*
036700160908     C     EXEVATB       BEGSR
036800160908     C*
036900160908     C                   EVAL      VATTRC = 'B'
037000160908     C                   EVAL      VATNOT = wVATNOT_B
037100160908     C                   exsr      wrivat                                       => scarico VAT
037200160908     C*
037300160908     C                   ENDSR
037400160909     C*----------------------------------------------------*
037500160909     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "I"
037600160909     C*----------------------------------------------------*
037700160909     C     EXEVATI       BEGSR
037800160909     C*
037900160909     C                   IF        %len(%trim(wVATNOT_I)) > 35
038000160909     C                   EVAL      VATTRC = 'I'
038100160909     C                   EVAL      VATNOT = %subst(%trim(wVATNOT_I):1:35)
038200160909     C                   exsr      wrivat                                       => scarico VAT
038300160909     C                   EVAL      VATTRC = 'J'
038400160909     C                   EVAL      VATNOT = %subst(%trim(wVATNOT_I):36)
038500160909     C                   exsr      wrivat                                       => scarico VAT
038600160909     C                   ELSE
038700160909     C                   EVAL      VATTRC = 'I'
038800160909     C                   EVAL      VATNOT = wVATNOT_I
038900160909     C                   exsr      wrivat                                       => scarico VAT
039000160909     C                   ENDIF
039100160909     C*
039200160909     C                   ENDSR
039300160909     C*----------------------------------------------------*
039400160909     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "J"
039500160909     C*----------------------------------------------------*
039600160909     C     EXEVATJ       BEGSR
039700160909     C*
039800160909     C                   IF        %len(%trim(wVATNOT_I)) <= 35
039900160909     C                   EVAL      VATTRC = 'J'
040000160909     C                   EVAL      VATNOT = wVATNOT_J
040100160909     C                   exsr      wrivat                                       => scarico VAT
040200160909     C                   ENDIF
040300160909     C*
040400160909     C                   ENDSR
040500160908     C*----------------------------------------------------*
040600160908     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "S"
040700160908     C*----------------------------------------------------*
040800160908     C     EXEVATS       BEGSR
040900160908     C*
041000160908     C                   EVAL      VATTRC = 'S'
041100160908     C                   EVAL      VATNOT = wVATNOT_S
041200160908     C                   exsr      wrivat                                       => scarico VAT
041300160908     C*
041400160908     C                   ENDSR
041500160908     C*----------------------------------------------------*
041600160908     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
041700160908     C*----------------------------------------------------*
041800160908     C     EXEVATE       BEGSR
041900160908     C*
042000160908     C                   EVAL      VATTRC = 'E'
042100160908     C                   EVAL      VATNOT = wVATNOT_E
042200160908     C                   exsr      wrivat                                       => scarico VAT
042300160908     C*
042400160908     C                   ENDSR
042500160908     C*----------------------------------------------------*
042600160908     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
042700160908     C*----------------------------------------------------*
042800160908     C     WRIVAT        BEGSR
042900160908     C*
043000160908     C* Scrivo solo se valorizzato qualcosa
043100160908     C                   IF        VATNOT <> *blanks
043200160908     C                   EVAL      VATFGS = VABFGS
043300160908     C                   EVAL      VATCCM = VABCCM
043400160908     C                   EVAL      VATLNP = VABLNP
043500160908     C                   EVAL      VATAAS = VABAAS
043600160908     C                   EVAL      VATNRS = VABNRS
043700160908     C                   EVAL      VATNSP = VABNSP
043800160908     C                   WRITE     FIVAT000
043900160908     C                   ADD       1             §CTROKVT
044000160908     C                   ENDIF
044100160908     C*
044200160908     C                   ENDSR
044300160908
044400160908
044500160908     C*----------------------------------------------------*
044600160908     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE ACCESSORIO
044700160908     C*----------------------------------------------------*
044800160908     C     PREVAT        BEGSR
044900160908     C*
045000160908     C* Compongo il nome del membro da dare al FIVATWWR
045100160908     C                   eval      parmbr = vlrhdl
045200160908     C                   movel     'M'           parmbr
045300160908     C                   eval      parccm = %subst(vlrKSC:2:7)
045400160908     C                   eval      paropz = '1'
045500160908     C*
045600160908     C* Effettuo la chiamata al CLLE preposto
045700160908     C                   call(e)   'TITVVTC'
045800160908     C                   parm                    parccm
045900160908     C                   parm                    parmbr
046000160908     C                   parm                    paropz
046100160908     C*
046200160908     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
046300160908     C                   if        %error
046400160908     C                   movel     '1'           chkcall
046500160908     C                   else
046600160908     C                   movel     '0'           chkcall
046700160908     C                   endif
046800160908     C*
046900160908     C                   ENDSR
047000160908
047100160908
047200160908      /TITLE Invio dei dati al punto operativo.
047300160908     C     invio         BEGSR
047400160908     C*
047500160908     C* 1° invio FIVAT
047600160908     C                   reset                   dscmz
047700160908     C                   move      vlrpoi        cmzdst
047800160908     C                   eval      cmzfld = 'FIVATWWR'
047900160908     C                   eval      cmzmbd = vlrhdl
048000160908     C                   eval      %subst(cmzmbd:1:1) = 'M'
048100160908     C***                if        prmfir = *blanks
048200160908     C                   eval      cmzfla = 'FIVAT00F'
048300160908     C                   eval      cmzmba = 'FIVAT00F'
048400160908     C***                else
048500160908     C***                eval      cmzfla = prmfir
048600160908     C***                eval      cmzmba = prmfir
048700160908     C***                endif
048800160908     C                   eval      cmznrr = *zeros
048900160908     C                   move      §ctrokvt      cmznrr
049000160908     C                   eval      cmzlba = vlrfl1
049100160908     C                   call(e)   'TIS711C'
049200160908     C                   parm                    dscmz
049300160908     C                   parm      *blanks       esito
049400160908     C                   if        %error
049500160908     C                             or cmzerr = '1'
049600160908     C                             or esito  = '1'
049700160908     C                   eval      wrkesito = '3'
049800160908     C                   else
049900160908     C*
050000160908     C* 2° invio FIVAB
050100160908     C                   reset                   dscmz
050200160908     C                   move      vlrpoi        cmzdst
050300160908     C                   eval      cmzfld = vlrfou
050400160908     C                   eval      cmzmbd = vlrhdl
050500160908     C                   eval      %subst(cmzmbd:1:1) = 'M'
050600160908     C***                if        prmfir = *blanks
050700160908     C                   eval      cmzfla = 'FIVAB00F'
050800160908     C                   eval      cmzmba = 'FIVAB00F'
050900160908     C***                else
051000160908     C***                eval      cmzfla = prmfir
051100160908     C***                eval      cmzmba = prmfir
051200160908     C***                endif
051300160908     C                   eval      cmznrr = *zeros
051400160908     C                   move      §ctrokvb      cmznrr
051500160908     C                   eval      cmzlba = vlrfl1
051600160908     C                   call(e)   'TIS711C'
051700160908     C                   parm                    dscmz
051800160908     C                   parm      *blanks       esito
051900160908     C                   if        %error
052000160908     C                             or cmzerr = '1'
052100160908     C                             or esito  = '1'
052200160908     C                   eval      wrkesito = '3'
052300160908     C                   endif
052400160908     C                   endif
052500160908     C*
052600160908     C                   ENDSR
052700160908     C***
052800010601
052900010601
053000010330     C*----------------------------------------------------*
053100030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
053200010330     C*----------------------------------------------------*
053300010330     C     INZVAR        BEGSR
053400010330     C*
053500030822     C* Inizializzo variabili di wrk
053600160902     C                   Z-ADD     *zeros        Num5_0            5 0
053700030822     C*
053800030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
053900160908     C                   CLEAR                   FIVAB000
054000160908     C                   CLEAR                   FIVAT000
054100160908     C                   CLEAR                   wVABNOT
054200160902     C                   CLEAR                   wVATNOT_A
054300160902     C                   CLEAR                   wVATNOT_B
054400160902     C                   CLEAR                   wVATNOT_E
054500150420     C                   CLEAR                   wVATNOT_I
054600160908     C                   CLEAR                   wVATNOT_J
054700150420     C                   CLEAR                   wVATNOT_S
054800030822     C                   CLEAR                   SkSplitCSV
054900160902     C                   CLEAR                   wFlgCAS
055000030822     C                   EVAL      i = 1
055100030822     C*
055200030822     C* Reimposto i valori di default
055300030822     C                   EXSR      DEFCAM
055400030822     C*
055500010330     C                   ENDSR
055600160902
055700160902
055800010330     C*----------------------------------------------------*
055900030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
056000010330     C*----------------------------------------------------*
056100010330     C     DEFCAM        BEGSR
056200110706     C*
056300160906     C                   SETOFF                                       7172
056400030822     C*
056500030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
056600030715     C* e delimitatore testo.
056700030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
056800030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
056900030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
057000150420     C                   IF        %subst(vlrppt:5:1) <> *blanks
057100150420     C                   EVAL      pptCSV = %subst(vlrppt:5:1)
057200150420     C                   ELSE
057300150420     C                   EVAL      pptCSV = ';'
057400150420     C                   ENDIF
057500150420     C                   IF        %subst(vlrppt:6:1) <> *blanks
057600150420     C                   EVAL      pptCHR = %subst(vlrppt:6:1)
057700150420     C                   ELSE
057800150420     C                   EVAL      pptCHR = ' '
057900150420     C                   ENDIF
058000090403     C*
058100090403     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
058200160902     C                   EVAL      CharSOS = CharNUM
058300030822     C*
058400030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
058500030715     C                   IF        %subst(vlrppt:1:1) = 'S'
058600160902     C                   SETON                                        50        * OUT => Download
058700030715     C                   ELSE
058800160902     C                   SETOFF                                       50        * OUT => File WW
058900030715     C                   ENDIF
059000030822     C*
059100030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
059200030822     C                   EVAL      posDaDft = 1
059300030822     C                   EVAL      posADft  = 0
059400030822     C                   EVAL      wGiroDft = 0
059500030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
059600030822     C                             posDaDft > 0
059700030822     C*
059800030822     C* Gestisco il 1° giro
059900030822     C                   IF        wGiroDft = 0
060000030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
060100030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
060200030822     C* Incremento il contatore dei "giri"
060300030822     C                   EVAL      wGiroDft = 1
060400030822     C                   ELSE
060500030822     C                   EVAL      posDaDft = posADft
060600030822     C                   ENDIF
060700030822     C* Eseguo lo scan x trovare la fine del campo corrente
060800030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
060900030822     C*
061000030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
061100030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
061200030822     C                   IF        posDaDft > 0 AND
061300030822     C                             posADft  > 0
061400110706     C* RMN
061500110706     C                   IF        %subst(
061600110706     C                             %subst(vlrppt:posDaDft+1:
061700110706     C                             posADft-posDaDft-1):1:3)
061800110706     C                             = 'RMN'
061900110706     C                   IF        %trim(%subst(
062000110706     C                             %subst(vlrppt:posDaDft+1:
062100110706     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062200110706     C                             = '*NSP'
062300160902     C                   SETON                                        71
062400110706     C                   ENDIF
062500110706     C                   ENDIF
062600060331     C* FGS
062700060331     C                   IF        %subst(
062800060331     C                             %subst(vlrppt:posDaDft+1:
062900060331     C                             posADft-posDaDft-1):1:3)
063000060331     C                             = 'FGS'
063100060331     C                   EVAL      PiStr=%trim(%subst(
063200060331     C                             %subst(vlrppt:posDaDft+1:
063300060331     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063400060331     C                   EXSR      CHKNUM
063500060331     C                   IF        PiInt=*on
063600060331     C                   Z-ADD     PiVal         VABFGS
063700060331     C                   ENDIF
063800060331     C                   ENDIF
063900030822     C* NCL
064000030822     C                   IF        %subst(
064100030822     C                             %subst(vlrppt:posDaDft+1:
064200030822     C                             posADft-posDaDft-1):1:3)
064300030822     C                             = 'NCL'
064400030822     C                   EVAL      PiStr=%trim(%subst(
064500030822     C                             %subst(vlrppt:posDaDft+1:
064600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064700030822     C                   EXSR      CHKNUM
064800030822     C                   IF        PiInt=*on
064900030822     C                   Z-ADD     PiVal         VABNCL
065000030822     C                   ENDIF
065100030822     C                   ENDIF
065200030822     C* CCM
065300030822     C                   IF        %subst(
065400030822     C                             %subst(vlrppt:posDaDft+1:
065500030822     C                             posADft-posDaDft-1):1:3)
065600030822     C                             = 'CCM'
065700030822     C                   EVAL      PiStr=%trim(%subst(
065800030822     C                             %subst(vlrppt:posDaDft+1:
065900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066000030822     C                   EXSR      CHKNUM
066100030822     C                   IF        PiInt=*on
066200030822     C                   Z-ADD     PiVal         VABCCM
066300030822     C                   ENDIF
066400030822     C                   ENDIF
066500030822     C* LNP
066600030822     C                   IF        %subst(
066700030822     C                             %subst(vlrppt:posDaDft+1:
066800030822     C                             posADft-posDaDft-1):1:3)
066900030822     C                             = 'LNP'
067000030822     C                   EVAL      PiStr=%trim(%subst(
067100030822     C                             %subst(vlrppt:posDaDft+1:
067200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067300030822     C                   EXSR      CHKNUM
067400030822     C                   IF        PiInt=*on
067500030822     C                   Z-ADD     PiVal         VABLNP
067600030822     C                   ENDIF
067700030822     C                   ENDIF
067800030822     C* NRS
067900030822     C                   IF        %subst(
068000030822     C                             %subst(vlrppt:posDaDft+1:
068100030822     C                             posADft-posDaDft-1):1:3)
068200030822     C                             = 'NRS'
068300030822     C                   EVAL      PiStr=%trim(%subst(
068400030822     C                             %subst(vlrppt:posDaDft+1:
068500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068600030822     C                   EXSR      CHKNUM
068700030822     C                   IF        PiInt=*on
068800030822     C                   Z-ADD     PiVal         VABNRS
068900030822     C                   ENDIF
069000030822     C                   ENDIF
069100160906     C* CTR
069200030822     C                   IF        %subst(
069300030822     C                             %subst(vlrppt:posDaDft+1:
069400030822     C                             posADft-posDaDft-1):1:3)
069500030822     C                             = 'CTR'
069600030822     C                   EVAL      PiStr=%trim(%subst(
069700030822     C                             %subst(vlrppt:posDaDft+1:
069800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069900030822     C                   EXSR      CHKNUM
070000030822     C                   IF        PiInt=*on
070100160906     C                   Z-ADD     PiVal         VABCTR
070200030822     C                   ENDIF
070300030822     C                   ENDIF
070400160906     C* CTE (ovvero CTR per Estero)
070500160906     C                   IF        %subst(
070600160906     C                             %subst(vlrppt:posDaDft+1:
070700160906     C                             posADft-posDaDft-1):1:3)
070800160906     C                             = 'CTE'
070900160906     C                   EVAL      PiStr=%trim(%subst(
071000160906     C                             %subst(vlrppt:posDaDft+1:
071100160906     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071200160906     C                   EXSR      CHKNUM
071300160906     C                   IF        PiInt=*on
071400160906     C                   Z-ADD     PiVal         wVABCTR_E
071500160906     C                   SETON                                        72
071600160906     C                   ENDIF
071700160906     C                   ENDIF
071800030822     C* PKB
071900030822     C                   IF        %subst(
072000030822     C                             %subst(vlrppt:posDaDft+1:
072100030822     C                             posADft-posDaDft-1):1:3)
072200030822     C                             = 'PKB'
072300030822     C                   EVAL      PiStr=%trim(%subst(
072400030822     C                             %subst(vlrppt:posDaDft+1:
072500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072600030822     C                   EXSR      CHKNUM
072700030822     C                   IF        PiNum=*on
072800030822     C                   Z-ADD     PiVal         VABPKB
072900030822     C                   ENDIF
073000030822     C                   ENDIF
073100030822     C* VLB
073200030822     C                   IF        %subst(
073300030822     C                             %subst(vlrppt:posDaDft+1:
073400030822     C                             posADft-posDaDft-1):1:3)
073500030822     C                             = 'VLB'
073600030822     C                   EVAL      PiStr=%trim(%subst(
073700030822     C                             %subst(vlrppt:posDaDft+1:
073800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073900030822     C                   EXSR      CHKNUM
074000030822     C                   IF        PiNum=*on
074100030822     C                   Z-ADD     PiVal         VABVLB
074200030822     C                   ENDIF
074300030822     C                   ENDIF
074400030822     C* QFT
074500030822     C                   IF        %subst(
074600030822     C                             %subst(vlrppt:posDaDft+1:
074700030822     C                             posADft-posDaDft-1):1:3)
074800030822     C                             = 'QFT'
074900030822     C                   EVAL      PiStr=%trim(%subst(
075000030822     C                             %subst(vlrppt:posDaDft+1:
075100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075200030822     C                   EXSR      CHKNUM
075300030822     C                   IF        PiNum=*on
075400030822     C                   Z-ADD     PiVal         VABQFT
075500030822     C                   ENDIF
075600030822     C                   ENDIF
075700030822     C* CBO
075800030822     C                   IF        %subst(
075900030822     C                             %subst(vlrppt:posDaDft+1:
076000030822     C                             posADft-posDaDft-1):1:3)
076100030822     C                             = 'CBO'
076200030822     C                   EVAL      VABCBO=%trim(%subst(
076300030822     C                             %subst(vlrppt:posDaDft+1:
076400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076500030822     C                   ENDIF
076600030822     C* TSP
076700030822     C                   IF        %subst(
076800030822     C                             %subst(vlrppt:posDaDft+1:
076900030822     C                             posADft-posDaDft-1):1:3)
077000030822     C                             = 'TSP'
077100030822     C                   EVAL      VABTSP=%trim(%subst(
077200030822     C                             %subst(vlrppt:posDaDft+1:
077300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077400030822     C                   ENDIF
077500030822     C* VAS
077600030822     C                   IF        %subst(
077700030822     C                             %subst(vlrppt:posDaDft+1:
077800030822     C                             posADft-posDaDft-1):1:3)
077900030822     C                             = 'VAS'
078000030822     C                   EVAL      VABVAS=%trim(%subst(
078100030822     C                             %subst(vlrppt:posDaDft+1:
078200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078300030822     C                   ENDIF
078400030822     C* VCA
078500030822     C                   IF        %subst(
078600030822     C                             %subst(vlrppt:posDaDft+1:
078700030822     C                             posADft-posDaDft-1):1:3)
078800030822     C                             = 'VCA'
078900030822     C                   EVAL      VABVCA=%trim(%subst(
079000030822     C                             %subst(vlrppt:posDaDft+1:
079100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079200030822     C                   ENDIF
079300030822     C* GCA
079400030822     C                   IF        %subst(
079500030822     C                             %subst(vlrppt:posDaDft+1:
079600030822     C                             posADft-posDaDft-1):1:3)
079700030822     C                             = 'GCA'
079800030822     C                   EVAL      VABGCA=%trim(%subst(
079900030822     C                             %subst(vlrppt:posDaDft+1:
080000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080100030822     C                   ENDIF
080200030822     C* CTM
080300030822     C                   IF        %subst(
080400030822     C                             %subst(vlrppt:posDaDft+1:
080500030822     C                             posADft-posDaDft-1):1:3)
080600030822     C                             = 'CTM'
080700030822     C                   EVAL      VABCTM=%trim(%subst(
080800030822     C                             %subst(vlrppt:posDaDft+1:
080900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081000030822     C                   ENDIF
081100030822     C* FFD
081200030822     C                   IF        %subst(
081300030822     C                             %subst(vlrppt:posDaDft+1:
081400030822     C                             posADft-posDaDft-1):1:3)
081500030822     C                             = 'FFD'
081600030822     C                   EVAL      VABFFD=%trim(%subst(
081700030822     C                             %subst(vlrppt:posDaDft+1:
081800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081900030822     C                   ENDIF
082000030822     C* VAD
082100030822     C                   IF        %subst(
082200030822     C                             %subst(vlrppt:posDaDft+1:
082300030822     C                             posADft-posDaDft-1):1:3)
082400030822     C                             = 'VAD'
082500030822     C                   EVAL      VABVAD=%trim(%subst(
082600030822     C                             %subst(vlrppt:posDaDft+1:
082700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082800030822     C                   ENDIF
082900030822     C* GMA
083000030822     C                   IF        %subst(
083100030822     C                             %subst(vlrppt:posDaDft+1:
083200030822     C                             posADft-posDaDft-1):1:3)
083300030822     C                             = 'GMA'
083400030822     C                   EVAL      VABGMA=%trim(%subst(
083500030822     C                             %subst(vlrppt:posDaDft+1:
083600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
083700030822     C                   ENDIF
083800030822     C* GGA
083900030822     C                   IF        %subst(
084000030822     C                             %subst(vlrppt:posDaDft+1:
084100030822     C                             posADft-posDaDft-1):1:3)
084200030822     C                             = 'GGA'
084300030822     C                   EVAL      VABGGA=%trim(%subst(
084400030822     C                             %subst(vlrppt:posDaDft+1:
084500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
084600030822     C                   ENDIF
084700030822     C* GVA
084800030822     C                   IF        %subst(
084900030822     C                             %subst(vlrppt:posDaDft+1:
085000030822     C                             posADft-posDaDft-1):1:3)
085100030822     C                             = 'GVA'
085200030822     C                   EVAL      VABGVA=%trim(%subst(
085300030822     C                             %subst(vlrppt:posDaDft+1:
085400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
085500030822     C                   ENDIF
085600030822     C* TC1
085700030822     C                   IF        %subst(
085800030822     C                             %subst(vlrppt:posDaDft+1:
085900030822     C                             posADft-posDaDft-1):1:3)
086000030822     C                             = 'TC1'
086100030822     C                   EVAL      VABTC1=%trim(%subst(
086200030822     C                             %subst(vlrppt:posDaDft+1:
086300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
086400030822     C                   ENDIF
086500030822     C* TC2
086600030822     C                   IF        %subst(
086700030822     C                             %subst(vlrppt:posDaDft+1:
086800030822     C                             posADft-posDaDft-1):1:3)
086900030822     C                             = 'TC2'
087000030822     C                   EVAL      VABTC2=%trim(%subst(
087100030822     C                             %subst(vlrppt:posDaDft+1:
087200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
087300030822     C                   ENDIF
087400030822     C* ...
087500030822     C                   ENDIF
087600030822     C                   ENDDO
087700020204     C*
087800010330     C                   ENDSR
087900010607     C*----------------------------------------------------*
088000021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
088100010607     C*----------------------------------------------------*
088200010607     C     IMPVAB        BEGSR
088300010607     C*
088400010607     C                   SETOFF                                       3132
088500030515     C*
088600010607     C                   MOVEL     datcor        VABAAS
088700010607     C                   MOVE      datcor        VABMGS
088800160902     C*
088900160902     C* Dopo aver splittato nei vari campi eseguo 1 prima "normalizzazione" x eliminare
089000160902     C* i caratteri di delimitazione testo e gli apici
089100160902     C                   EVAL      SkSplitCSV = SkSplitCSV_5
089200160902     C                   EVAL      i = 1
089300160902     C                   DOW       i < %elem(SkSplitCSV)
089400160902     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
089500160902     C     '''':' '      XLATE     SkSplitCSV(i) SkSplitCSV(i)
089600160902     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
089700160902     C                   EVAL      i = i + 1
089800160902     C                   ENDDO
089900010607     C*
090000030715     C**********
090100160902     C* Valorizzo la lista delle INTESTAZIONI colonne cliente
090200030715     C**********
090300030715     C                   IF        wGiro = *zeros
090400160902     C*
090500160902     C                   EVAL      wGiro = 1
090600160902     C                   EVAL      SkSplitFLD = SkSplitCSV
090700160902     C*
090800160902     C                   ELSE
090900160902     C*
091000160902     C**********
091100160902     C* Elaboro le righe di DATI cliente
091200160902     C**********
091300160902     C                   EVAL      wGiro = 2
091400030715     C*
091500030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
091600030715     C                   EVAL      i = 1
091700030715     C                   DOW       i < %elem(SkSplitFLD)
091800030715     C*
091900030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
092000030715     C                   IF        SkSplitFLD(i) <> *blanks AND
092100030715     C                             SkSplitCSV(i) <> *blanks
092200130624     C*** ATB
092300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
092400150420     C                   EVAL      VABATB = SkSplitCSV(i)
092500030721     C                   ENDIF
092600030715     C*** RSD
092700160902     C                   IF        %trim(SkSplitFLD(i)) =
092800160909     C                             'RAG SOCIALE'
092900030715     C                   EVAL      VABRSD = SkSplitCSV(i)
093000030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
093100030715     C     '@':'A'       XLATE     VABRSD        VABRSD
093200030715     C* ==
093300030715     C                   ENDIF
093400030721     C*** RD2
093500160902     C                   IF        %trim(SkSplitFLD(i)) =
093600160909     C                             'VABRD2'
093700160902     C                   EVAL      VABRD2 = SkSplitCSV(i)
093800160902     C                   ENDIF
093900030715     C*** IND
094000160902     C                   IF        %trim(SkSplitFLD(i)) =
094100160909     C                             'INDIRIZZO'
094200030715     C                   EVAL      VABIND = SkSplitCSV(i)
094300030715     C                   ENDIF
094400030715     C*** LOD
094500160902     C                   IF        %trim(SkSplitFLD(i)) =
094600160909     C                             'LOCALITA'
094700160909     C                   EVAL      VABLOD = SkSplitCSV(i)
094800030715     C                   ENDIF
094900030715     C*** PRD
095000160902     C                   IF        %trim(SkSplitFLD(i)) =
095100160909     C                             'PR'
095200030715     C                   EVAL      VABPRD = SkSplitCSV(i)
095300030715     C                   ENDIF
095400160902     C*** CAD
095500160902     C                   IF        %trim(SkSplitFLD(i)) =
095600160909     C                             'CAP'
095700160902     C                   EVAL      VABCAD = SkSplitCSV(i)
095800160902     C                   ENDIF
095900030721     C*** NZD
096000160902     C                   IF        %trim(SkSplitFLD(i)) =
096100160909     C                             'VABNZD'
096200030721     C                   EVAL      VABNZD = SkSplitCSV(i)
096300030721     C                   ENDIF
096400160902     C*** RMA - 1
096500160909     C                   IF        %trim(SkSplitFLD(i)) = 'RIF MIT'
096600160902     C                   EVAL      wVABRMA_1 = SkSplitCSV(i)
096700150515     C                   ENDIF
096800160902     C*** RMA - 2
096900160908     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA_2'
097000160902     C                   EVAL      wVABRMA_2 = SkSplitCSV(i)
097100160902     C                   ENDIF
097200030715     C*** NOT
097300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
097400030715     C                   EVAL      VABNOT = SkSplitCSV(i)
097500030715     C                   ENDIF
097600030715     C*** NT2
097700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
097800030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
097900030715     C                   ENDIF
098000030715     C*** VCA
098100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
098200030715     C                   EVAL      VABVCA = SkSplitCSV(i)
098300030715     C                   ENDIF
098400030715     C*** VAS
098500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
098600030715     C                   EVAL      VABVAS = SkSplitCSV(i)
098700030715     C                   ENDIF
098800030715     C*** FFD
098900160902     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
099000030715     C                   EVAL      VABFFD = SkSplitCSV(i)
099100030715     C                   ENDIF
099200160902     C*** CBO
099300160902     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
099400160902     C                   EVAL      VABCBO = SkSplitCSV(i)
099500150420     C                   ENDIF
099600030721     C*** GC1
099700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
099800150420     C                   IF        %len(%trim(SkSplitCSV(i))) = 1
099900150420     C                   MOVEL     *blanks       wGC               1
100000150420     C                   EVAL      wGC = %trim(SkSplitCSV(i))
100100150420     C                   IF        SkSplitCSV(i) = 'M' OR
100200150420     C                             SkSplitCSV(i) = 'P'
100300150420     C                   MOVE(P)   wGC           VABGC1
100400150420     C                   ELSE
100500150420     C                   MOVEL(P)  wGC           VABGC1
100600150420     C                   ENDIF
100700150420     C                   ENDIF
100800030721     C                   ENDIF
100900030721     C*** GC2
101000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
101100120314     C                   IF        %len(%trim(SkSplitCSV(i))) = 1
101200120314     C                   MOVEL     *blanks       wGC               1
101300120314     C                   EVAL      wGC = %trim(SkSplitCSV(i))
101400120314     C                   IF        SkSplitCSV(i) = 'M' OR
101500120314     C                             SkSplitCSV(i) = 'P'
101600120314     C                   MOVE(P)   wGC           VABGC2
101700120314     C                   ELSE
101800120314     C                   MOVEL(P)  wGC           VABGC2
101900120314     C                   ENDIF
102000120314     C                   ENDIF
102100030721     C                   ENDIF
102200030721     C*** TSP
102300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
102400030721     C                   EVAL      VABTSP = SkSplitCSV(i)
102500030721     C                   ENDIF
102600030721     C*** NAS
102700160902     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
102800030721     C                   EVAL      VABNAS = SkSplitCSV(i)
102900030721     C                   ENDIF
103000030721     C*** TIC
103100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
103200030721     C                   EVAL      VABTIC = SkSplitCSV(i)
103300030721     C                   ENDIF
103400030721     C*** GCA
103500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
103600030721     C                   EVAL      VABGCA = SkSplitCSV(i)
103700030721     C                   ENDIF
103800030721     C*** XCO
103900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
104000030721     C                   EVAL      VABXCO = SkSplitCSV(i)
104100030721     C                   ENDIF
104200030721     C*** CTM
104300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
104400030721     C                   EVAL      VABCTM = SkSplitCSV(i)
104500030721     C                   ENDIF
104600030721     C*** TCR
104700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
104800030721     C                   EVAL      VABTCR = SkSplitCSV(i)
104900030721     C                   ENDIF
105000030721     C*** CTS
105100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
105200030721     C                   EVAL      VABCTS = SkSplitCSV(i)
105300030721     C                   ENDIF
105400030721     C*** FTM
105500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
105600030721     C                   EVAL      VABFTM = SkSplitCSV(i)
105700030721     C                   ENDIF
105800030721     C*** VAD
105900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
106000030721     C                   EVAL      VABVAD = SkSplitCSV(i)
106100030721     C                   ENDIF
106200030721     C*** GMA
106300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
106400030721     C                   EVAL      VABGMA = SkSplitCSV(i)
106500030721     C                   ENDIF
106600030721     C*** GGA
106700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
106800030721     C                   EVAL      VABGGA = SkSplitCSV(i)
106900030721     C                   ENDIF
107000030721     C*** GVA
107100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
107200030721     C                   EVAL      VABGVA = SkSplitCSV(i)
107300030721     C                   ENDIF
107400030721     C*** TC1
107500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
107600030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
107700030721     C                   ENDIF
107800030721     C*** TC2
107900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
108000030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
108100030721     C                   ENDIF
108200030721     C*** SCL
108300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
108400030721     C                   EVAL      VABSCL = SkSplitCSV(i)
108500030721     C                   ENDIF
108600030721     C*** RMO
108700160902     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
108800030721     C                   EVAL      VABRMO = SkSplitCSV(i)
108900030721     C                   ENDIF
109000160902     C*** CMO
109100160902     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
109200160902     C                   EVAL      VABCMO = SkSplitCSV(i)
109300160902     C                   ENDIF
109400030721     C*** NMO
109500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
109600030721     C                   EVAL      VABNMO = SkSplitCSV(i)
109700030721     C                   ENDIF
109800160908     C*** NOT_A
109900160908     C                   IF        %trim(SkSplitFLD(i)) =
110000160909     C                             'VATNOT_A'
110100160908     C                   EVAL      wVATNOT_A = SkSplitCSV(i)
110200160908     C                   ENDIF
110300160902     C*** NOT_B
110400160902     C                   IF        %trim(SkSplitFLD(i)) =
110500160909     C                             'TEL'
110600160902     C                   EVAL      wVATNOT_B = SkSplitCSV(i)
110700160902     C                   ENDIF
110800150420     C*** NOT_I
110900160902     C                   IF        %trim(SkSplitFLD(i)) =
111000160909     C                             'EMAIL'
111100150420     C                   EVAL      wVATNOT_I = SkSplitCSV(i)
111200150420     C                   ENDIF
111300150420     C*** NOT_S
111400160909     C                   IF        %trim(SkSplitFLD(i)) =
111500160909     C                             'TEL'
111600150420     C                   EVAL      wVATNOT_S = SkSplitCSV(i)
111700150420     C                   ENDIF
111800160909     C*** NOT_E
111900160909     C                   IF        %trim(SkSplitFLD(i)) =
112000160909     C                             'BARCODE'
112100160909     C                   EVAL      wVATNOT_E = SkSplitCSV(i)
112200160909     C                   ENDIF
112300030721     C*
112400070705     C* Reperisco quindi i campi numerici...
112500030721     C*** RMN
112600160908     C                   IF        %trim(SkSplitFLD(i)) =
112700160909     C                             'RIFERIMENTO'
112800030721     C                   EVAL      PiStr=SkSplitCSV(i)
112900030721     C                   EXSR      CHKNUM
113000030721     C                   IF        PiInt=*on
113100030721     C                   Z-ADD     PiVal         VABRMN
113200160909     C                   Z-ADD     PiVal         VABNSP
113300030721     C                   ELSE
113400030721     C                   SETON                                        32
113500030721     C                   EVAL      VABRMN = 1
113600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113700160909     C                             + ' ' + 'VABRMN VABNSP'
113800030721     C                   ENDIF
113900030721     C                   ENDIF
114000030721     C*** NSP
114100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
114200030721     C                   EVAL      PiStr=SkSplitCSV(i)
114300030721     C                   EXSR      CHKNUM
114400100517     C                   IF        PiInt=*on
114500030721     C                   Z-ADD     PiVal         VABNSP
114600030721     C                   ELSE
114700030721     C                   SETON                                        32
114800030721     C                   EVAL      VABNSP = *zeros
114900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115000030721     C                             + ' ' + 'VABNSP'
115100030721     C                   ENDIF
115200030721     C                   ENDIF
115300160902     C*** NCL
115400160909     C                   IF        %trim(SkSplitFLD(i)) = 'COLLI'
115500030721     C                   EVAL      PiStr=SkSplitCSV(i)
115600030721     C                   EXSR      CHKNUM
115700030721     C                   IF        PiInt=*on
115800030721     C                   Z-ADD     PiVal         VABNCL
115900030721     C                   ELSE
116000030721     C                   SETON                                        32
116100030721     C                   EVAL      VABNCL = *zeros
116200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116300030721     C                             + ' ' + 'VABNCL'
116400030721     C                   ENDIF
116500030721     C                   ENDIF
116600030721     C*** PKB
116700160909     C                   IF        %trim(SkSplitFLD(i)) = 'PESO'
116800030721     C                   EVAL      PiStr=SkSplitCSV(i)
116900030721     C                   EXSR      CHKNUM
117000030721     C                   IF        PiNum=*on
117100030721     C                   Z-ADD     PiVal         VABPKB
117200030721     C                   ELSE
117300030721     C                   SETON                                        32
117400030721     C                   EVAL      VABPKB = *zeros
117500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117600030721     C                             + ' ' + 'VABPKB'
117700030721     C                   ENDIF
117800030721     C                   ENDIF
117900160908     C*** CAS
118000160909     C                   IF        %trim(SkSplitFLD(i)) = 'CONTR.'
118100160915     C                   IF        SkSplitCSV(i) <> *blanks AND
118200160915     C                             SkSplitCSV(i) <> *zeros  AND
118300170503     C                             SkSplitCSV(i) <> '0,000' AND
118400170503     C                             SkSplitCSV(i) <> '0,0'
118500160908     C                   EVAL      wFlgCAS = 1
118600160908     C                   ENDIF
118700160902     C                   EVAL      PiStr=SkSplitCSV(i)
118800160902     C                   EXSR      CHKNUM
118900160902     C                   IF        PiNum=*on
119000160902     C                   Z-ADD     PiVal         VABCAS
119100160902     C                   MOVEL     'EUR'         VABVCA
119200160902     C                   ELSE
119300160902     C                   SETON                                        32
119400160902     C                   EVAL      VABCAS = *zeros
119500160902     C                   EVAL      vinmsg = %trimr(vinmsg)
119600160902     C                             + ' ' + 'VABCAS'
119700160902     C                   ENDIF
119800160902     C                   ENDIF
119900170503     C*** IAS
120000170503     C                   IF        %trim(SkSplitFLD(i)) = 'ASSICUR'
120100170503     C                   IF        SkSplitCSV(i) <> *blanks AND
120200170503     C                             SkSplitCSV(i) <> *zeros  AND
120300170503     C                             SkSplitCSV(i) <> '0,000' AND
120400170503     C                             SkSplitCSV(i) <> '0,0'
120500170503     C                   EVAL      wFlgCAS = 1
120600170503     C                   ENDIF
120700170503     C                   EVAL      PiStr=SkSplitCSV(i)
120800170503     C                   EXSR      CHKNUM
120900170503     C                   IF        PiNum=*on
121000170503     C                   Z-ADD     PiVal         VABIAS
121100170503     C                   MOVEL     'EUR'         VABVAS
121200170503     C                   ELSE
121300170503     C                   SETON                                        32
121400170503     C                   EVAL      VABIAS = *zeros
121500170503     C                   EVAL      vinmsg = %trimr(vinmsg)
121600170503     C                             + ' ' + 'VABIAS'
121700170503     C                   ENDIF
121800170503     C                   ENDIF
121900030721     C*** CCM
122000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
122100030721     C                   EVAL      PiStr=SkSplitCSV(i)
122200030721     C                   EXSR      CHKNUM
122300030721     C                   IF        PiInt=*on      AND
122400030721     C                             PiVal<=9999999 AND
122500030721     C                             PiVal>*zeros
122600030721     C                   Z-ADD     PiVal         VABCCM
122700030721     C                   ELSE
122800030721     C                   SETON                                        32
122900030721     C                   EVAL      VABCCM = *zeros
123000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123100030721     C                             + ' ' + 'VABCCM'
123200030721     C                   ENDIF
123300030721     C                   ENDIF
123400030721     C*** LNP
123500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
123600030721     C                   EVAL      PiStr=SkSplitCSV(i)
123700030721     C                   EXSR      CHKNUM
123800030721     C                   IF        PiInt=*on    AND
123900030721     C                             PiVal<=999   AND
124000030721     C                             PiVal>*zeros
124100030721     C                   Z-ADD     PiVal         VABLNP
124200030721     C                   ELSE
124300030721     C                   SETON                                        32
124400030721     C                   EVAL      VABLNP = *zeros
124500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124600030721     C                             + ' ' + 'VABLNP'
124700030721     C                   ENDIF
124800030721     C                   ENDIF
124900030721     C*** NRS
125000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
125100030721     C                   EVAL      PiStr=SkSplitCSV(i)
125200030721     C                   EXSR      CHKNUM
125300030721     C                   IF        PiInt=*on AND
125400030721     C                             PiVal<=99
125500030721     C                   Z-ADD     PiVal         VABNRS
125600030721     C                   ELSE
125700030721     C                   SETON                                        32
125800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125900030721     C                             + ' ' + 'VABNRS'
126000030721     C                   ENDIF
126100030721     C                   ENDIF
126200030721     C*** LNA
126300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
126400120314     C                   IF        %trim(SkSplitCSV(i)) <> *zeros  AND
126500120314     C                             %trim(SkSplitCSV(i)) <> '0'     AND
126600120314     C                             %trim(SkSplitCSV(i)) <> '000'   AND
126700120314     C                             %trim(SkSplitCSV(i)) <> *blanks
126800030721     C                   EVAL      PiStr=SkSplitCSV(i)
126900030721     C                   EXSR      CHKNUM
127000030721     C                   IF        PiInt=*on    AND
127100030721     C                             PiVal<=999   AND
127200030721     C                             PiVal>*zeros
127300030721     C                   Z-ADD     PiVal         VABLNA
127400030721     C                   ELSE
127500030721     C                   SETON                                        32
127600030721     C                   EVAL      VABLNA = *zeros
127700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127800030721     C                             + ' ' + 'VABLNA'
127900120314     C                   ENDIF
128000030721     C                   ENDIF
128100030721     C                   ENDIF
128200030721     C*** CTR
128300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
128400030721     C                   EVAL      PiStr=SkSplitCSV(i)
128500030721     C                   EXSR      CHKNUM
128600030721     C                   IF        PiInt=*on  AND
128700030721     C                             PiVal<=999
128800030721     C                   Z-ADD     PiVal         VABCTR
128900030721     C                   ELSE
129000030721     C                   SETON                                        32
129100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129200030721     C                             + ' ' + 'VABCTR'
129300030721     C                   ENDIF
129400030721     C                   ENDIF
129500030721     C*** IAS
129600160902     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
129700030721     C                   EVAL      PiStr=SkSplitCSV(i)
129800030721     C                   EXSR      CHKNUM
129900030721     C                   IF        PiNum=*on
130000030721     C                   Z-ADD     PiVal         VABIAS
130100030721     C                   ELSE
130200030721     C                   SETON                                        32
130300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130400030721     C                             + ' ' + 'VABIAS'
130500030721     C                   ENDIF
130600030721     C                   ENDIF
130700030721     C*** VLB
130800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
130900030721     C                   EVAL      PiStr=SkSplitCSV(i)
131000030721     C                   EXSR      CHKNUM
131100030721     C                   IF        PiNum=*on
131200030721     C                   Z-ADD     PiVal         VABVLB
131300030721     C                   ELSE
131400030721     C                   SETON                                        32
131500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131600030721     C                             + ' ' + 'VABVLB'
131700030721     C                   ENDIF
131800030721     C                   ENDIF
131900030721     C*** QFT
132000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
132100030721     C                   EVAL      PiStr=SkSplitCSV(i)
132200030721     C                   EXSR      CHKNUM
132300030721     C                   IF        PiNum=*on
132400030721     C                   Z-ADD     PiVal         VABQFT
132500030721     C                   ELSE
132600030721     C                   SETON                                        32
132700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132800030721     C                             + ' ' + 'VABQFT'
132900030721     C                   ENDIF
133000030721     C                   ENDIF
133100030721     C*** NCD
133200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
133300030721     C                   EVAL      PiStr=SkSplitCSV(i)
133400030721     C                   EXSR      CHKNUM
133500030721     C                   IF        PiInt=*on      AND
133600030721     C                             PiVal<=9999999
133700030721     C                   Z-ADD     PiVal         VABNCD
133800030721     C                   ELSE
133900030721     C                   SETON                                        32
134000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134100030721     C                             + ' ' + 'VABNCD'
134200030721     C                   ENDIF
134300030721     C                   ENDIF
134400030721     C*** NCA
134500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
134600030721     C                   EVAL      PiStr=SkSplitCSV(i)
134700030721     C                   EXSR      CHKNUM
134800030721     C                   IF        PiInt=*on      AND
134900030721     C                             PiVal<=9999999
135000030721     C                   Z-ADD     PiVal         VABNCA
135100030721     C                   ELSE
135200030721     C                   SETON                                        32
135300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135400030721     C                             + ' ' + 'VABNCA'
135500030721     C                   ENDIF
135600030721     C                   ENDIF
135700030721     C*** ZNC
135800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
135900030721     C                   EVAL      PiStr=SkSplitCSV(i)
136000030721     C                   EXSR      CHKNUM
136100030721     C                   IF        PiInt=*on AND
136200030721     C                             PiVal<=99
136300030721     C                   Z-ADD     PiVal         VABZNC
136400030721     C                   ELSE
136500030721     C                   SETON                                        32
136600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136700030721     C                             + ' ' + 'VABZNC'
136800030721     C                   ENDIF
136900030721     C                   ENDIF
137000030721     C*** DCR
137100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
137200030721     C                   EVAL      PiStr=SkSplitCSV(i)
137300030721     C                   EXSR      CHKNUM
137400030721     C                   IF        PiInt=*on       AND
137500030721     C                             PiVal<=99999999
137600030721     C                   Z-ADD     PiVal         VABDCR
137700030721     C                   ELSE
137800030721     C                   SETON                                        32
137900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138000030721     C                             + ' ' + 'VABDCR'
138100030721     C                   ENDIF
138200030721     C                   ENDIF
138300030721     C*** HCR
138400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
138500030721     C                   EVAL      PiStr=SkSplitCSV(i)
138600030721     C                   EXSR      CHKNUM
138700030721     C                   IF        PiInt=*on   AND
138800030721     C                             PiVal<=9999
138900030721     C                   Z-ADD     PiVal         VABHCR
139000030721     C                   ELSE
139100030721     C                   SETON                                        32
139200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
139300030721     C                             + ' ' + 'VABHCR'
139400030721     C                   ENDIF
139500030721     C                   ENDIF
139600030721     C*** VMD
139700160902     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
139800030721     C                   EVAL      PiStr=SkSplitCSV(i)
139900030721     C                   EXSR      CHKNUM
140000030721     C                   IF        PiNum=*on
140100030721     C                   Z-ADD     PiVal         VABVMD
140200030721     C                   ELSE
140300030721     C                   SETON                                        32
140400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
140500030721     C                             + ' ' + 'VABVMD'
140600030721     C                   ENDIF
140700030721     C                   ENDIF
140800030721     C*** ANT
140900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
141000030721     C                   EVAL      PiStr=SkSplitCSV(i)
141100030721     C                   EXSR      CHKNUM
141200030721     C                   IF        PiInt=*on        AND
141300030721     C                             PiVal<=999999999
141400030721     C                   Z-ADD     PiVal         VABANT
141500030721     C                   ELSE
141600030721     C                   SETON                                        32
141700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
141800030721     C                             + ' ' + 'VABANT'
141900030721     C                   ENDIF
142000030721     C                   ENDIF
142100030715     C*
142200030715     C                   ENDIF
142300030715     C                   EVAL      i = i + 1
142400030715     C                   ENDDO
142500070705     C*
142600160902     C* Considerazioni particolari "fuori ciclo"
142700110706     C*
142800160908     C* Verifica campi "particolari" VAB
142900160908     C                   EXSR      CHKVAB
143000160908     C*
143100030715     C                   ENDIF
143200020204     C*
143300010607     C                   ENDSR
143400010607     C*----------------------------------------------------*
143500160908
143600160908
143700160908     C*----------------------------------------------------*
143800160908     C*  CONSIDERAZIONI SU CAMPI PARTICOLARI VAB
143900160908     C*----------------------------------------------------*
144000160908     C     CHKVAB        BEGSR
144100160908     C*
144200160908     C*** RMN
144300160908     C                   IF        *in71
144400160908     C                   EVAL      VABRMN = VABNSP
144500160908     C                   ENDIF
144600160908     C                   IF        VABRMN = *zeros
144700160908     C                   EVAL      VABRMN = 1
144800160908     C                   ENDIF
144900160908     C*** NZD
145000160908     C                   IF        VABNZD = 'IT'  OR
145100160908     C                             VABNZD = 'ITA' OR
145200160908     C                             VABNZD = 'Ita' OR
145300160908     C                             VABNZD = 'ita'
145400160908     C                   EVAL      VABNZD = *blanks
145500160908     C                   ENDIF
145600160908     C*** CTR
145700160908     C                   IF        *in72
145800160908     C                   IF        VABNZD <> *blanks
145900160908     C                   EVAL      VABCTR =  wVABCTR_E
146000160908     C                   ENDIF
146100160908     C                   ENDIF
146200160908     C*** CBO
146300160908     C                   IF        VABCBO = *blanks
146400160908     C                   EVAL      VABCBO = '1'
146500160908     C                   ENDIF
146600160908     C*** NCL
146700160908     C                   IF        VABNCL = *zeros
146800160908     C                   EVAL      VABNCL = 1
146900160908     C                   ENDIF
147000160908     C*** CCM
147100160908     C                   IF        VABCCM = *zeros
147200160908     C                   EVAL      PiStr=VLRKSC
147300160908     C                   EXSR      CHKNUM
147400160908     C                   IF        PiInt=*on
147500160908     C                   Z-ADD     PiVal         VABCCM
147600160908     C                   ELSE
147700160908     C                   SETON                                        31
147800160908     C                   ENDIF
147900160908     C                   ENDIF
148000160908     C*** LNP
148100160908     C                   IF        VABLNP = *zeros
148200160908     C                   IF        VLRPOI = 046 OR
148300160908     C                             VLRPOI = 789 OR
148400160908     C                             VLRPOI = 999
148500160908     C                   MOVEL     VABCCM        VABLNP
148600160908     C                   ELSE
148700160908     C                   EVAL      VABLNP = VLRPOI
148800160908     C                   ENDIF
148900160908     C                   ENDIF
149000160908     C*** LNP
149100160908     C                   IF        VABFGS = *zeros
149200160908     C                   EVAL      VABFGS = VABLNP
149300160908     C                   ENDIF
149400160908     C*** FGS / LNP
149500160908     C                   IF        VABFGS = *zeros OR
149600160908     C                             VABLNP = *zeros
149700160908     C                   SETON                                        31
149800160908     C                   ENDIF
149900160908     C*** RMA
150000160908     C                   IF        wVABRMA_1 <> *blanks
150100160908     C                   EVAL      VABRMA = wVABRMA_1
150200160908     C                   ELSE
150300160920     C                   IF        wVABRMA_2 <> *blanks
150400160908     C                   EVAL      VABRMA = wVABRMA_2
150500160908     C                   ENDIF
150600160920     C                   ENDIF
150700160908     C*** CAS
150800160908     C                   IF        wFlgCAS = *zeros
150900160908     C                   EVAL      VABCAS = *zeros
151000160908     C                   EVAL      VABVCA = *blanks
151100160908     C                   ENDIF
151200160908     C*** NOT
151300160908     C                   IF        VABNOT = *blanks AND wVABNOT <> *blanks
151400160908     C                   EVAL      VABNOT = wVABNOT
151500160908     C                   ENDIF
151600160908     C*
151700160908     C* Se provincia nn valorizzata la reperisco
151800160908     C* tramite TISI95R a seconda dei dati d instradamento presenti
151900160908     C                   IF        VABPRD = *blanks
152000160908     C                   CLEAR                   TISI95DS
152100160908     C                   EVAL      I95TCN = '3'
152200160908     C                   Z-ADD     datcor        I95DAT
152300160908     C                   EVAL      I95NAR = VABNZD
152400160908     C                   EVAL      I95CAP = VABCAD
152500160908     C                   EVAL      I95LOC = VABLOD
152600160908     C                   CALL      'TISI95R'
152700160908     C                   PARM                    TISI95DS
152800160908     C                   EVAL      VABPRD = O95PRV
152900160908     C                   ENDIF
153000160908     C*
153100160908     C* Eseguo routine finale x considerazioni specifiche su importi/divise
153200160908     C                   EXSR      CHKIMPDIV
153300160908     C*
153400160908     C                   ENDSR
153500160908     C***
153600010601
153700020204
153800020204     C*----------------------------------------------------*
153900020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
154000020204     C*----------------------------------------------------*
154100020204     C     CHKIMPDIV     BEGSR
154200060524     C*
154300060524     C* Verifico il codice bolla in base al tipo porto e alla presenza o meno del contrassegno
154400160906     C                   IF        vabCAS  > *zeros OR
154500160906     C                             wFlgCAS > *zeros
154600060524     C                   IF        vabCBO = '1'
154700060524     C                   EVAL      vabCBO = '4'
154800060524     C                   ENDIF
154900060524     C                   IF        vabCBO = '2'
155000060524     C                   EVAL      vabCBO = '6'
155100060524     C                   ENDIF
155200060524     C                   ENDIF
155300020204     C*
155400020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
155500020204     C                   Z-ADD     *zeros        wrkDec            9 9
155600020204     C*
155700020204     C* Come prima cosa effettuo considerazioni sulla divisa
155800020204     C                   IF        vabIAS > *zeros
155900150420     C                   IF        vabVAS =  *blanks
156000150420     C                   EVAL      vabVAS =  'EUR'
156100020204     C                   ENDIF
156200020204     C                   ENDIF
156300020204     C*
156400020204     C                   IF        vabCAS > *zeros
156500150420     C                   IF        vabVCA <> *blanks
156600150420     C                   EVAL      vabVCA =  'EUR'
156700020204     C                   ENDIF
156800020204     C                   ENDIF
156900020204     C*
157000020204     C                   IF        vabVMD > *zeros
157100150420     C                   IF        vabVAD <> *blanks
157200150420     C                   EVAL      vabVAD =  'EUR'
157300020204     C                   ENDIF
157400020204     C                   ENDIF
157500020204     C*
157600020204     C                   ENDSR
157700020204     C***
157800010330
157900010330
158000010330     C*----------------------------------------------------*
158100010330     C*  CONTROLLO NUMERICITA' CAMPI
158200010330     C*----------------------------------------------------*
158300010330     C     CHKNUM        BEGSR
158400010330     C*
158500010606     C                   IF        PiDecChr = *blanks
158600030715     C                   EVAL      PiDecChr = CharNUM
158700010606     C                   ENDIF
158800010606     C*
158900010606     C                   CALL(e)   'ISNUMERIC'
159000010330     C                   PARM                    PiStr            30
159100010606     C                   PARM                    PiDecChr          1
159200010330     C                   PARM      *ZEROS        PiVal            30 9
159300010330     C                   PARM      '0'           PiInt             1
159400010330     C                   PARM      '0'           PiNum             1
159500010330     C                   IF        %error
159600010606     C                   EVAL      PiNum=*off
159700010330     C                   ENDIF
159800010330     C*
159900010330     C                   ENDSR
160000010330     C***
160100160906
160200160906
160300160906      /TITLE Valorizzazione Progressivo Applicazione
160400160906     C     calprog       begsr
160500160906     C*
160600160906     C                   movel     *blanks       dwlisv            2
160700160906     C                   movel     *all'0'       dwlprg           10
160800160906     C                   z-add     *zeros        wrkprg            8 0
160900160906     C*
161000160906     C                   eval      dwlisv = vlrisv
161100160906     C*
161200170324     C     *start        setll     tis7prgf
161300160906     C                   read(e)   tis7prgf
161400160906     C                   if        not %error
161500160906     C                   eval      dwlprg = f_tis7prgf
161600160906     C*
161700160906     C                   move(p)   dwlprg        wrkprg
161800160906     C                   add       1             wrkprg
161900160906     C                   move(p)   wrkprg        dwlprg
162000160906     C                   movel     dwlisv        dwlprg
162100160906     C*
162200160906     C                   eval      f_tis7prgf = dwlprg
162300160906     C                   update    tis7prg0
162400170324     C                   else
162500170324     C                   eval      f_tis7prgf = 'XX00000000'
162600160906     C                   endif
162700160906     C*
162800160906     C                   endsr
162900050421
163000160908
163100160908     C     *pssr         BEGSR
163200160908     C*
163300160908     C                   if        %open(tivin00r)
163400160908     C                   close     tivin00r
163500160908     C                   endif
163600160908     C                   if        %open(fivabwwr)
163700160908     C                   close     fivabwwr
163800160908     C                   endif
163900160908     C                   if        %open(fivatwwr)
164000160908     C                   close     fivatwwr
164100160908     C                   endif
164200160908     C*
164300160908     C* Effettuo la chiamata al CLLE preposto
164400160908     C  N50              call(e)   'TITVVTC'
164500160908     C                   parm                    parccm
164600160908     C                   parm                    parmbr
164700160908     C                   parm      '2'           paropz
164800160908     C*
164900160908     C                   eval      wrkesito = '2'
165000160908     C*
165100160908     C                   seton                                        LR
165200160908     C*
165300160908     C                   ENDSR     '*CANCL'
165400160908     C***
165500100419
165600100419
165700160902     C     normCSV       BEGSR
165800160902     C*
165900160902     C                   z-add     *zeros        wLenVINDTA        4 0
166000160902     C                   z-add     *zeros        wFlgVINDTA        1 0
166100160902     C                   setoff                                       80
166200160902     C*
166300160902     C                   dow       wLenVINDTA < %len(vindta)
166400160902     C                   eval      wLenVINDTA = wLenVINDTA + 1
166500160902     C                   movel     *blanks       wAlfa1            1
166600160902     C                   eval      wAlfa1 = %subst(vindta:wLenVINDTA:1)
166700160902     C*
166800160902     C* Se byte inferiore a HEX40 è un byte "non stampabile" per cui considero *blank
166900160902     C                   if        wAlfa1 < X'40'
167000160902     C                   eval      %subst(vindta:wLenVINDTA:1) = *blanks
167100160902     C                   endif
167200160902     C*
167300160902     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
167400160902     C                   if        wAlfa1 <> CharCSV and
167500160902     C                             wAlfa1 <> CharTXT and
167600160902     C                             wAlfa1 <> CharNUM
167700160902     C                   z-add     1             wFlgVINDTA
167800160902     C                   endif
167900160902     C*
168000160902     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
168100160902     C* separatori campi
168200160902     C                   if        CharTXT <> *blanks
168300160902     C                   if        wAlfa1 = CharTXT
168400160902     C                   if        not *in80
168500160902     C                   seton                                        80
168600160902     C                   else
168700160902     C                   setoff                                       80
168800160902     C                   endif
168900160902     C                   endif
169000160902     C                   if        wAlfa1 = CharCSV and *in80 = *on
169100160902     C                   eval      %subst(vindta:wLenVINDTA:1) = CharSOS
169200160902     C                   endif
169300160902     C                   endif
169400160902     C*
169500160902     C                   enddo
169600100419     C*
169700100419     C                   ENDSR
169800100419     C***
169900150420
170000150420
170100150420      /TITLE Scrittura record in file TIVGD00F (file VAS generico download)
170200160902     C     wriVAB_VGD    BEGSR
170300160909     C*
170400160909     C* Verifica campi "particolari" VAB
170500160909     C                   exsr      repNSP
170600160909     C*
170700160909     C                   setoff                                       45
170800160909     C                   if        depSPE  = *zeros OR
170900160909     C                             depSPE <> vabNSP
171000160909     C                   seton                                        45
171100160909     C                   eval      depSPE  = vabNSP
171200160909     C                   endif
171300160909     C*
171400160909     C                   if        not *in31
171500150420     C*
171600160902     C* Se primo giro allora scrivo testata
171700160902     C                   if        wVGD_Head = *zeros
171800160902     C                   eval      wVGD_Head = 1
171900160902     C                   eval      wBufferOut =
172000160902     C                             %trim(pptCHR)+'VABCCM' +%trim(pptCHR)+pptCSV+
172100160902     C                             %trim(pptCHR)+'VABLNP' +%trim(pptCHR)+pptCSV+
172200160902     C                             %trim(pptCHR)+'VABCTR' +%trim(pptCHR)+pptCSV+
172300160902     C                             %trim(pptCHR)+'VABRSD' +%trim(pptCHR)+pptCSV+
172400160902     C                             %trim(pptCHR)+'VABRD2' +%trim(pptCHR)+pptCSV+
172500160902     C                             %trim(pptCHR)+'VABIND' +%trim(pptCHR)+pptCSV+
172600160902     C                             %trim(pptCHR)+'VABLOD' +%trim(pptCHR)+pptCSV+
172700160902     C                             %trim(pptCHR)+'VABCAD' +%trim(pptCHR)+pptCSV+
172800160902     C                             %trim(pptCHR)+'VABPRD' +%trim(pptCHR)+pptCSV+
172900160902     C                             %trim(pptCHR)+'VABCBO' +%trim(pptCHR)+pptCSV+
173000160902     C                             %trim(pptCHR)+'VABRMO' +%trim(pptCHR)+pptCSV+
173100160902     C                             %trim(pptCHR)+'VABRMN' +%trim(pptCHR)+pptCSV+
173200160902     C                             %trim(pptCHR)+'VABRMA' +%trim(pptCHR)+pptCSV+
173300160902     C                             %trim(pptCHR)+'VABNAS' +%trim(pptCHR)+pptCSV+
173400160902     C                             %trim(pptCHR)+'VABNCL' +%trim(pptCHR)+pptCSV+
173500160902     C                             %trim(pptCHR)+'VABPKB' +%trim(pptCHR)+pptCSV+
173600160902     C                             %trim(pptCHR)+'VABCAS' +%trim(pptCHR)+pptCSV+
173700160902     C                             %trim(pptCHR)+'VABVCA' +%trim(pptCHR)+pptCSV+
173800170503     C                             %trim(pptCHR)+'VABIAS' +%trim(pptCHR)+pptCSV+
173900170503     C                             %trim(pptCHR)+'VABVAS' +%trim(pptCHR)+pptCSV+
174000160902     C                             %trim(pptCHR)+'VABFFD' +%trim(pptCHR)+pptCSV+
174100160902     C                             %trim(pptCHR)+'VABIAS' +%trim(pptCHR)+pptCSV+
174200160902     C                             %trim(pptCHR)+'VABVAS' +%trim(pptCHR)+pptCSV+
174300160902     C                             %trim(pptCHR)+'VABNOT' +%trim(pptCHR)+pptCSV+
174400160902     C                             %trim(pptCHR)+'VABNRC' +%trim(pptCHR)+pptCSV+
174500160902     C                             %trim(pptCHR)+'VABTRC' +%trim(pptCHR)+pptCSV+
174600160902     C                             %trim(pptCHR)+'VABEMD' +%trim(pptCHR)+pptCSV+
174700160902     C                             %trim(pptCHR)+'VABCEL' +%trim(pptCHR)
174800160902     C                   exsr      wriVGD
174900160902     C                   endif
175000160902     C*
175100160902     C* Quindi scrivo dati spedizione
175200160909     C   45              eval      wBufferOut =
175300150420     C                             %trim(pptCHR)+%trim(%editc(VABCCM:'X'))
175400150420     C                                                    +%trim(pptCHR)+pptCSV+
175500150420     C                             %trim(pptCHR)+%trim(%editc(VABLNP:'X'))
175600150420     C                                                    +%trim(pptCHR)+pptCSV+
175700150420     C                             %trim(pptCHR)+%trim(%editc(VABCTR:'X'))
175800150420     C                                                    +%trim(pptCHR)+pptCSV+
175900150420     C                             %trim(pptCHR)+%trim(VABRSD)
176000150420     C                                                    +%trim(pptCHR)+pptCSV+
176100160902     C                             %trim(pptCHR)+%trim(VABRD2)
176200160902     C                                                    +%trim(pptCHR)+pptCSV+
176300150420     C                             %trim(pptCHR)+%trim(VABIND)
176400150420     C                                                    +%trim(pptCHR)+pptCSV+
176500150420     C                             %trim(pptCHR)+%trim(VABLOD)
176600150420     C                                                    +%trim(pptCHR)+pptCSV+
176700150420     C                             %trim(pptCHR)+%trim(VABCAD)
176800150420     C                                                    +%trim(pptCHR)+pptCSV+
176900150420     C                             %trim(pptCHR)+%trim(VABPRD)
177000150420     C                                                    +%trim(pptCHR)+pptCSV+
177100150420     C                             %trim(pptCHR)+%trim(VABCBO)
177200150420     C                                                    +%trim(pptCHR)+pptCSV+
177300150420     C                             %trim(pptCHR)+%trim(VABRMO)
177400150420     C                                                    +%trim(pptCHR)+pptCSV+
177500150420     C                             %trim(pptCHR)+%trim(%editc(VABRMN:'4'))
177600150420     C                                                    +%trim(pptCHR)+pptCSV+
177700150420     C                             %trim(pptCHR)+%trim(VABRMA)
177800150420     C                                                    +%trim(pptCHR)+pptCSV+
177900150420     C                             %trim(pptCHR)+%trim(VABNAS)
178000150420     C                                                    +%trim(pptCHR)+pptCSV+
178100150420     C                             %trim(pptCHR)+%trim(%editc(VABNCL:'4'))
178200150420     C                                                    +%trim(pptCHR)+pptCSV+
178300150420     C                             %trim(pptCHR)+%trim(%editc(VABPKB:'4'))
178400150420     C                                                    +%trim(pptCHR)+pptCSV+
178500150420     C                             %trim(pptCHR)+%trim(%editc(VABCAS:'4'))
178600150420     C                                                    +%trim(pptCHR)+pptCSV+
178700150420     C                             %trim(pptCHR)+%trim(VABVCA)
178800150420     C                                                    +%trim(pptCHR)+pptCSV+
178900170503     C                             %trim(pptCHR)+%trim(%editc(VABIAS:'4'))
179000170503     C                                                    +%trim(pptCHR)+pptCSV+
179100170503     C                             %trim(pptCHR)+%trim(VABVAS)
179200170503     C                                                    +%trim(pptCHR)+pptCSV+
179300150420     C                             %trim(pptCHR)+%trim(VABFFD)
179400150420     C                                                    +%trim(pptCHR)+pptCSV+
179500150420     C                             %trim(pptCHR)+%trim(%editc(VABIAS:'4'))
179600150420     C                                                    +%trim(pptCHR)+pptCSV+
179700150420     C                             %trim(pptCHR)+%trim(VABVAS)
179800150420     C                                                    +%trim(pptCHR)+pptCSV+
179900150421     C                             %trim(pptCHR)+%trim(VABNOT)
180000150421     C                                                    +%trim(pptCHR)+pptCSV+
180100160902     C                             %trim(pptCHR)+%trim(wVATNOT_A)
180200160902     C                                                    +%trim(pptCHR)+pptCSV+
180300160902     C                             %trim(pptCHR)+%trim(wVATNOT_B)
180400160902     C                                                    +%trim(pptCHR)+pptCSV+
180500150420     C                             %trim(pptCHR)+%trim(wVATNOT_I)
180600150420     C                                                    +%trim(pptCHR)+pptCSV+
180700150420     C                             %trim(pptCHR)+%trim(wVATNOT_S)
180800150420     C                                                    +%trim(pptCHR)
180900160909     C   45              exsr      wriVGD
181000160909     C*
181100160909     C                   endif
181200150420     C*
181300150420     C                   ENDSR
181400150420     C*------------------------------------------------------------------------*
181500150420
181600150420
181700150420
181800150420      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
181900150420     C     wriVGD        BEGSR
182000150420     C*
182100150420     C                   clear                   tivgd000
182200150420     C                   eval      vgdDTA = wBufferOut
182300150420     C                   eval      vgdTIP = 'EB'
182400150420     C                   eval      vgdKSU = vlrKSC
182500150420     C                   eval      vgdTSC = 'WW'
182600150420     C                   eval      vgdDAT = datcor
182700160906     C                   eval      vgdPRG = f_tis7prgf
182800160920     C                   eval      vgdPGM = procname
182900160906     C                   eval      vgdSTO = '?'
183000150420     C                   write     tivgd000
183100150420     C*
183200150420     C                   ENDSR
183300150420     C*------------------------------------------------------------------------*
183400160902
183500160902
183600160902     C*----------------------------------------------------*
183700160902     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
183800160902     C*----------------------------------------------------*
183900160902     C     SR_SPLIT      BEGSR
184000160902     C*
184100160902     C* Chiamo il *pgm d utilità x lo "split" della riga delle intestazioni
184200160902     C                   CALL      'XSPLIT2'
184300160902     C                   PARM                    InStringa
184400160902     C                   PARM                    InSepara
184500160902     C                   PARM      '5'           InTypeOut
184600160902     C                   PARM      ''            wSkParam
184700160902     C                   PARM                    OutErrore
184800160902     C                   PARM                    OutMsg
184900160902     C                   MOVEA     wSkParam      SkSplitCSV_5
185000160902     C*
185100160902     C                   ENDSR
185200150420
185300150420
185400000613     C     *inzsr        BEGSR
185500990910     C*
185600990910     C     *entry        plist
185700990920     C                   parm                    tivlrds
185800990921     C                   parm      wrkesito      esito
185900000724     C                   parm                    prmlit
186000000710     C                   parm                    prmfir
186100010330     C*
186200010330     C* CALCOLA LA DATA CORRENTE
186300150420     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
186400150420     C                   eval      datcor = %dec(%date() : *ISO)
186500000613     C*
186600000613     C                   ENDSR
186700000613     C***
