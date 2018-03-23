000100030715      /TITLE Upload via Internet: traduzione in FIVABWWR.
000200150420     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300150420     H DECEDIT('0,') DATEDIT(*DMY.)
000400000313     F*
000500170316     Ftabel00f  if   e           k disk
000600990910     Ftivin00r  uF   E             DISK    usropn
000700140114     FTIVGD00F  O    E             DISK
000800160906     Ftis7prgf  uF   E             DISK    rename(tis7prgf:tis7prg0)
000900160906     F                                     prefix(f_)
001000160908     FFIVABwwr  O    E             DISK    usropn
001100160908     FFIVATwwr  O    E             DISK    usropn
001200000313     D*
001300010330     D*----------------------------------------------------
001400010330     D* DICHIARAZIOINE VARIABILI DI WRK
001500010330     D*----------------------------------------------------
001600160908     D dscmz         e ds                  inz
001700010330     D psds           sds
001800010330     D  procname         *PROC
001900010330     D tivlrds       e ds                  extname(tivlr00f)
002000140114     D tisi95ds      e ds
002100170316     D ds15          e ds
002200010330     D esito           s              1
002300010330     D prmlit          s             10
002400010330     D prmfir          s             10
002500150420     D pptCHR          s              1    inz
002600150420     D pptCSV          s              1    inz
002700160902     D wBufferOut      s           2048    inz
002800160902     D wVATNOT_A       s             35    inz
002900160902     D wVATNOT_B       s             35    inz
003000160902     D wVATNOT_E       s             35    inz
003100150420     D wVATNOT_I       s             70    inz
003200160908     D wVATNOT_J       s             70    inz
003300150420     D wVATNOT_S       s             35    inz
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
004500170316
004600170316     D*------------
004700170316     D jNAZ            s              5  0 INZ(*zeros)
004800170316     D skNAZISO        S              3    DIM(1000)
004900170316     D skNAZBAR        S              3    DIM(1000)
005000160902
005100160908
005200160908     D*------------------
005300160908     D* DS REPERIMENTO NUMERATORE
005400160908     D*------------------
005500160908     D trul33ds      e ds                  inz
005600160908     D*------------------
005700160908     D* DS ARCHITETTURA
005800160908     D*------------------
005900160908     D KPJBA         e ds                  inz
006000160908
006100160902
006200160902     D*------------------
006300160902     D* VARIABILI X LA CHIAMATA AL *PGM UTILITA' "XSPLIT"
006400160902     D*------------------
006500160902     D InStringa       S          65535A   VARYING                              (stringa input)
006600160902     D InSepara        S             10A                                        (separatore)
006700160902     D InTypeOut       S              1A                                        (tipo output)
006800160902     D wSkParam        S          65535A                                        (output)
006900160902     D OutErrore       S              1A                                        (flag errore)
007000160902     D OutMsg          S             80A                                        (messaggio errore)
007100160902     D SkSplitCSV_5    S            256    DIM(256)
007200160902
007300160902
007400030715     D*------------------
007500030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
007600030715     D*------------------
007700160902     D SkSplitFLD      S                   LIKE(SkSplitCSV_5) DIM(256)
007800160902     D SkSplitCSV      S                   LIKE(SkSplitCSV_5) DIM(256)
007900160902     D CharCSV         S              1
008000160902     D CharTXT         S              1
008100160902     D CharNUM         S              1
008200160902     D CharSOS         S              1
008300160902     D i               s              3  0 INZ(1)
008400030715     D wGiro           s              1  0 INZ(*zeros)
008500030822     D*------------------
008600030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
008700030822     D*------------------
008800160902     D posDaDft        S              3  0 INZ(*zeros)
008900160902     D posADft         S              3  0 INZ(*zeros)
009000160902     D wGiroDft        s              1  0 INZ(*zeros)
009100160908
009200160908
009300031201     D*------------------
009400031201     D* Costanti
009500031201     D*------------------
009600160902     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
009700160908     D MAIU            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
009800150420
009900010330
010000010330
010100990921     C                   reset                   esito
010200990921     C                   reset                   wrkesito
010300010601     C*
010400170316     C                   exsr      cartab
010500160908     C                   exsr      defcam
010600160908     C   50              exsr      calprog
010700150420     C                   exsr      rwfile
010800160908     C   50              exsr      strvgd
010900160906     C                   exsr      endela
011000160906     C*
011100010601     C                   seton                                        lr
011200160906
011300160906
011400160906     C*--------------------------------------------------------
011500160906     C* STRVGD - "LIBERO" LA TRANSAZIONE (GRUPPO) CORRENTE    *
011600160906     C*--------------------------------------------------------
011700160906     C     STRVGD        BEGSR
011800160906     C*
011900160906     C
012000170119     C/EXEC SQL
012100170227     C+ UPDATE TIVGD00F SET VGDSTO = 'R'
012200170119     C+ WHERE VGDSTO = '?'     AND VGDPRG = :VGDPRG AND
012300170119     C+       VGDTIP = :VGDTIP AND VGDKSU = :VGDKSU AND
012400170119     C+       VGDTSC = :VGDTSC
012500170119     C/END-EXEC
012600160906     C
012700160906     C*
012800160906     C                   ENDSR
012900160906     C***
013000160902
013100010601
013200010601     C*--------------------------------------------------------
013300160902     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
013400010601     C*--------------------------------------------------------
013500010601     C     ENDELA        BEGSR
013600080222     C*
013700080222     C* Effettuo lancio TISI95 solo x chiusura
013800080222     C                   CLEAR                   TISI95DS
013900080222     C                   EVAL      I95TLA = 'C'
014000080222     C                   CALL      'TISI95R'
014100080222     C                   PARM                    TISI95DS
014200160908     C*
014300160908     C* Effettuo la chiamata al CLLE preposto
014400160908     C  N50              call(e)   'TITVVTC'
014500160908     C                   parm                    parccm
014600160908     C                   parm                    parmbr
014700160908     C                   parm      '2'           paropz
014800000613     C*
014900010601     C                   ENDSR
015000010601     C***
015100010601
015200010601
015300010330     C*--------------------------------------------------------
015400150420     C* RWFILE  LEGGE TIVIN00R E SCRIVE FIVABWWF              *
015500010330     C*--------------------------------------------------------
015600150420     C     RWFILE        BEGSR
015700010330     C*
015800010330     C                   if        not %open(tivin00r)
015900010330     C                   open      tivin00r
016000010330     C                   endif
016100160908     C*
016200160908     C* Eseguo operazioni di aggiunta nuovo membro file FNVAT
016300160908     C  N50              exsr      prevat
016400160908     C   50              eval      chkcall = '0'
016500160908     C*
016600160908     C                   if        chkcall = '0'
016700160908     C*
016800160908     C                   if        not %open(fivabwwr)
016900160908     C                   open      fivabwwr
017000160908     C                   endif
017100160908     C*
017200160908     C                   if        not %open(fivatwwr)
017300160908     C                   open      fivatwwr
017400160908     C                   endif
017500010330     C*
017600160908     C                   clear                   §CTROKVB          7 0
017700160908     C                   clear                   §CTROKVT          7 0
017800160908     C                   clear                   §CTRMO            7 0
017900160908     C                   clear                   §CTRNO            7 0
018000010330     C*
018100030822     C                   do        *HIVAL
018200010330     C*
018300030822     C                   read      tivin00r                               70
018400160902     C*
018500160902     C* Salvo il buffer orginale ricevuto dal cliente
018600160902     C                   eval      s_vindta = vindta
018700160902     C*
018800160902     C                   if        *in70 = *off and
018900160902     C                             vindta <> *blanks and
019000160902     C                             (vinflg = *blanks
019100160902     C                              or vinflg = '0'
019200160902     C                              or vinflg = '2')
019300160902     C*
019400160902     C* Controlli di "base" sui dati grezzi
019500160902     C                   exsr      normCSV
019600040510     C*
019700160902     C                   if        wFlgVINDTA = 1
019800010330     C*
019900010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
020000010711     C                   if        vinflg = *blanks or vinflg = '0'
020100010711     C                   clear                   vinmsg
020200010711     C                   endif
020300160902     C*
020400160902 xxx C* Porto eventualmente da minuscolo in maiuscolo le INTESTAZIONI colonne cliente
020500160908 xxx C***  minu:MAIU     XLATE     vindta        vindta
020600160902     C*
020700160902     C* Splitto i dati
020800160902     C                   CLEAR                   SkSplitCSV_5
020900160902     C                   EVAL      InStringa = vindta
021000160902     C                   EVAL      InSepara  = CharCSV
021100160902     C                   EXSR      SR_SPLIT
021200160902     C*
021300160902     C*****************************
021400160909 xxx C* A questo punto verifico che le colonne mandatorie siano valorizzate correttamente
021500170522     C                   if        %len(%trim(SkSplitCSV_5(2)))  > 2 and
021600170522     C                             %len(%trim(SkSplitCSV_5(7)))  > 3 and
021700170522     C                             %len(%trim(SkSplitCSV_5(8)))  > 3 and
021800170522     C                             %len(%trim(SkSplitCSV_5(10))) > 3 and
021900170522     C                             %len(%trim(SkSplitCSV_5(12))) > 2
022000160902     C*****************************
022100010601     C*
022200160902     C* Se OK => procedo con elaborazione
022300010330     C                   exsr      inzvar
022400010605     C                   exsr      impvab
022500010601     C*
022600010604     C* Ebbene...
022700160908     C                   if        wGiro > 1
022800160908     C  N31              add       1             §CTROKVB
022900160908     C   32              add       1             §CTRMO
023000160908     C   31              add       1             §CTRNO
023100160908     C*
023200160902     C   50              exsr      wriVAB_VGD
023300160908     C  N50              exsr      wriVAB_WW
023400150420     C                   endif
023500010604     C*
023600010604     C                   if        *in31 = *off and
023700010604     C                             *in32 = *off
023800010604     C                   eval      vinflg = '1'
023900010604     C                   else
024000010604     C                   eval      vinflg = '2'
024100010604     C                   endif
024200010601     C*
024300160902     C                   else
024400160902     C                   eval      vinflg = '1'
024500010604     C                   endif
024600160902     C*
024700160902     C                   else
024800160902     C                   eval      vinflg = '1'
024900160902     C                   endif
025000010604     C*
025100010330     C                   else
025200010330     C                   eval      vinflg = '1'
025300010330     C                   endif
025400010601     C*
025500160902     C                   eval      vindta = s_vindta
025600010601     C  N70              update    tivin000
025700010330     C*
025800030822     C  N70              enddo
025900010601     C
026000010601     C*
026100160902     C* Stabilisco l'esito della traduzione
026200160902     C                   select
026300160902     C*
026400160902     C* Se non ci sono record con errori => stato OK
026500160902     C                   when      §ctrno = 0 and
026600160902     C                             §ctrmo = 0
026700010330     C                   eval      wrkesito = '0'
026800160902     C*
026900160902     C* Se nessun record OK => errore totale
027000160908     C                   when      §ctrokvb = 0
027100160902     C                   eval      wrkesito = '2'
027200160902     C*
027300160902     C* Diversamente => errore parziale/warning
027400160902     C                   other
027500160902     C                   eval      wrkesito = '1'
027600160902     C*
027700160902     C                   endsl
027800160908     C*
027900160908     C                   endif
028000160908     C
028100160908     C                   if        chkcall = '1'
028200160908     C                   eval      wrkesito = '2'
028300160908     C                   endif
028400010330     C*
028500010330     C                   if        %open(tivin00r)
028600010330     C                   close     tivin00r
028700010330     C                   endif
028800160908     C*
028900160908     C                   if        %open(fivabwwr)
029000160908     C                   close     fivabwwr
029100160908     C                   endif
029200160908     C                   if        %open(fivatwwr)
029300160908     C                   close     fivatwwr
029400160908     C                   endif
029500160908     C*
029600160908     C                   if        not *in50        and
029700160908     C                             wrkesito <> '2'  and
029800160908     C                             vlrpoi <> *zeros
029900160908     C                   exsr      invio
030000160908     C                   endif
030100010330     C*
030200010330     C                   ENDSR
030300010330     C***
030400160908
030500160908
030600160908     C*----------------------------------------------------*
030700160908     C*  SCARICAMENTO BUFFER RECORDS VAB IN "WW"
030800160908     C*----------------------------------------------------*
030900160908     C     WRIVAB_WW     BEGSR
031000160908     C*
031100160908     C* Verifica campi "particolari" VAB
031200160908     C                   exsr      repNSP
031300160909     C*
031400160909     C                   setoff                                       45
031500160909     C                   if        depSPE  = *zeros OR
031600160909     C                             depSPE <> vabNSP
031700160909     C                   seton                                        45
031800160909     C                   eval      depSPE  = vabNSP
031900160909     C                   endif
032000160909     C*
032100160909     C                   if        not *in31
032200160909     C                   exsr      EXEVATE
032300160909     C   45              exsr      EXEVATA
032400160909     C   45              exsr      EXEVATB
032500160909     C   45              exsr      EXEVATI
032600160909     C   45              exsr      EXEVATJ
032700160909     C   45              exsr      EXEVATS
032800160909     C   45              write     fivab000                                     => scarico il VAB
032900160909     C                   endif
033000160908     C*
033100160908     C                   ENDSR
033200160908
033300160908
033400160908     C*----------------------------------------------------*
033500160908     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
033600160908     C*----------------------------------------------------*
033700160908     C     REPNSP        BEGSR
033800160908     C*
033900160909     C* NSP => Se necessario stacco un numeratore da AZNUM
034000160909     C                   if        VABNSP = *zeros
034100160908     C                   clear                   TRUL33DS
034200160908     C                   eval      I33OPE = *zeros
034300160908     C                   eval      I33CNU = 302
034400160908     C                   eval      I33NUM = 1
034500160908     C                   movel     TRUL33DS      KPJBU
034600160908     C                   call      'TRUL33R'
034700160908     C                   parm                    KPJBA
034800160908     C                   movel     KPJBU         TRUL33DS
034900160908     C                   if        O33ERR = *zeros
035000160908     C                   z-add     O33NRF        VABNSP
035100160908     C                   z-add     O33NRF        VATNSP
035200160908     C                   else
035300160908     C                   SETON                                        31
035400160908     C                   EVAL      vinmsg = %trimr(vinmsg)
035500160908     C                             + ' ' + 'VABNSP VATNSP'
035600160908     C                   endif
035700160909     C                   endif
035800160908     C*
035900160908     C                   ENDSR
036000160908
036100160908
036200160908     C*----------------------------------------------------*
036300160908     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
036400160908     C*----------------------------------------------------*
036500160908     C     EXEVATA       BEGSR
036600160908     C*
036700160908     C                   EVAL      VATTRC = 'A'
036800160908     C                   EVAL      VATNOT = wVATNOT_A
036900160908     C                   exsr      wrivat                                       => scarico VAT
037000160908     C*
037100160908     C                   ENDSR
037200160908     C*----------------------------------------------------*
037300160908     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
037400160908     C*----------------------------------------------------*
037500160908     C     EXEVATB       BEGSR
037600160908     C*
037700160908     C                   EVAL      VATTRC = 'B'
037800160908     C                   EVAL      VATNOT = wVATNOT_B
037900160908     C                   exsr      wrivat                                       => scarico VAT
038000160908     C*
038100160908     C                   ENDSR
038200160909     C*----------------------------------------------------*
038300160909     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "I"
038400160909     C*----------------------------------------------------*
038500160909     C     EXEVATI       BEGSR
038600160909     C*
038700160909     C                   IF        %len(%trim(wVATNOT_I)) > 35
038800160909     C                   EVAL      VATTRC = 'I'
038900160909     C                   EVAL      VATNOT = %subst(%trim(wVATNOT_I):1:35)
039000160909     C                   exsr      wrivat                                       => scarico VAT
039100160909     C                   EVAL      VATTRC = 'J'
039200160909     C                   EVAL      VATNOT = %subst(%trim(wVATNOT_I):36)
039300160909     C                   exsr      wrivat                                       => scarico VAT
039400160909     C                   ELSE
039500160909     C                   EVAL      VATTRC = 'I'
039600160909     C                   EVAL      VATNOT = wVATNOT_I
039700160909     C                   exsr      wrivat                                       => scarico VAT
039800160909     C                   ENDIF
039900160909     C*
040000160909     C                   ENDSR
040100160909     C*----------------------------------------------------*
040200160909     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "J"
040300160909     C*----------------------------------------------------*
040400160909     C     EXEVATJ       BEGSR
040500160909     C*
040600160909     C                   IF        %len(%trim(wVATNOT_I)) <= 35
040700160909     C                   EVAL      VATTRC = 'J'
040800160909     C                   EVAL      VATNOT = wVATNOT_J
040900160909     C                   exsr      wrivat                                       => scarico VAT
041000160909     C                   ENDIF
041100160909     C*
041200160909     C                   ENDSR
041300160908     C*----------------------------------------------------*
041400160908     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "S"
041500160908     C*----------------------------------------------------*
041600160908     C     EXEVATS       BEGSR
041700160908     C*
041800160908     C                   EVAL      VATTRC = 'S'
041900160908     C                   EVAL      VATNOT = wVATNOT_S
042000160908     C                   exsr      wrivat                                       => scarico VAT
042100160908     C*
042200160908     C                   ENDSR
042300160908     C*----------------------------------------------------*
042400160908     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
042500160908     C*----------------------------------------------------*
042600160908     C     EXEVATE       BEGSR
042700160908     C*
042800160908     C                   EVAL      VATTRC = 'E'
042900160908     C                   EVAL      VATNOT = wVATNOT_E
043000160908     C                   exsr      wrivat                                       => scarico VAT
043100160908     C*
043200160908     C                   ENDSR
043300160908     C*----------------------------------------------------*
043400160908     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
043500160908     C*----------------------------------------------------*
043600160908     C     WRIVAT        BEGSR
043700160908     C*
043800160908     C* Scrivo solo se valorizzato qualcosa
043900160908     C                   IF        VATNOT <> *blanks
044000160908     C                   EVAL      VATFGS = VABFGS
044100160908     C                   EVAL      VATCCM = VABCCM
044200160908     C                   EVAL      VATLNP = VABLNP
044300160908     C                   EVAL      VATAAS = VABAAS
044400160908     C                   EVAL      VATNRS = VABNRS
044500160908     C                   EVAL      VATNSP = VABNSP
044600160908     C                   WRITE     FIVAT000
044700160908     C                   ADD       1             §CTROKVT
044800160908     C                   ENDIF
044900160908     C*
045000160908     C                   ENDSR
045100160908
045200160908
045300160908     C*----------------------------------------------------*
045400160908     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE ACCESSORIO
045500160908     C*----------------------------------------------------*
045600160908     C     PREVAT        BEGSR
045700160908     C*
045800160908     C* Compongo il nome del membro da dare al FIVATWWR
045900160908     C                   eval      parmbr = vlrhdl
046000160908     C                   movel     'M'           parmbr
046100160908     C                   eval      parccm = %subst(vlrKSC:2:7)
046200160908     C                   eval      paropz = '1'
046300160908     C*
046400160908     C* Effettuo la chiamata al CLLE preposto
046500160908     C                   call(e)   'TITVVTC'
046600160908     C                   parm                    parccm
046700160908     C                   parm                    parmbr
046800160908     C                   parm                    paropz
046900160908     C*
047000160908     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
047100160908     C                   if        %error
047200160908     C                   movel     '1'           chkcall
047300160908     C                   else
047400160908     C                   movel     '0'           chkcall
047500160908     C                   endif
047600160908     C*
047700160908     C                   ENDSR
047800160908
047900160908
048000160908      /TITLE Invio dei dati al punto operativo.
048100160908     C     invio         BEGSR
048200160908     C*
048300160908     C* 1° invio FIVAT
048400160908     C                   reset                   dscmz
048500160908     C                   move      vlrpoi        cmzdst
048600160908     C                   eval      cmzfld = 'FIVATWWR'
048700160908     C                   eval      cmzmbd = vlrhdl
048800160908     C                   eval      %subst(cmzmbd:1:1) = 'M'
048900160908     C***                if        prmfir = *blanks
049000160908     C                   eval      cmzfla = 'FIVAT00F'
049100160908     C                   eval      cmzmba = 'FIVAT00F'
049200160908     C***                else
049300160908     C***                eval      cmzfla = prmfir
049400160908     C***                eval      cmzmba = prmfir
049500160908     C***                endif
049600160908     C                   eval      cmznrr = *zeros
049700160908     C                   move      §ctrokvt      cmznrr
049800160908     C                   eval      cmzlba = vlrfl1
049900160908     C                   call(e)   'TIS711C'
050000160908     C                   parm                    dscmz
050100160908     C                   parm      *blanks       esito
050200160908     C                   if        %error
050300160908     C                             or cmzerr = '1'
050400160908     C                             or esito  = '1'
050500160908     C                   eval      wrkesito = '3'
050600160908     C                   else
050700160908     C*
050800160908     C* 2° invio FIVAB
050900160908     C                   reset                   dscmz
051000160908     C                   move      vlrpoi        cmzdst
051100160908     C                   eval      cmzfld = vlrfou
051200160908     C                   eval      cmzmbd = vlrhdl
051300160908     C                   eval      %subst(cmzmbd:1:1) = 'M'
051400160908     C***                if        prmfir = *blanks
051500160908     C                   eval      cmzfla = 'FIVAB00F'
051600160908     C                   eval      cmzmba = 'FIVAB00F'
051700160908     C***                else
051800160908     C***                eval      cmzfla = prmfir
051900160908     C***                eval      cmzmba = prmfir
052000160908     C***                endif
052100160908     C                   eval      cmznrr = *zeros
052200160908     C                   move      §ctrokvb      cmznrr
052300160908     C                   eval      cmzlba = vlrfl1
052400160908     C                   call(e)   'TIS711C'
052500160908     C                   parm                    dscmz
052600160908     C                   parm      *blanks       esito
052700160908     C                   if        %error
052800160908     C                             or cmzerr = '1'
052900160908     C                             or esito  = '1'
053000160908     C                   eval      wrkesito = '3'
053100160908     C                   endif
053200160908     C                   endif
053300160908     C*
053400160908     C                   ENDSR
053500160908     C***
053600010601
053700010601
053800010330     C*----------------------------------------------------*
053900030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
054000010330     C*----------------------------------------------------*
054100010330     C     INZVAR        BEGSR
054200010330     C*
054300030822     C* Inizializzo variabili di wrk
054400160902     C                   Z-ADD     *zeros        Num5_0            5 0
054500030822     C*
054600030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
054700160908     C                   CLEAR                   FIVAB000
054800160908     C                   CLEAR                   FIVAT000
054900160908     C                   CLEAR                   wVABNOT
055000160902     C                   CLEAR                   wVATNOT_A
055100160902     C                   CLEAR                   wVATNOT_B
055200160902     C                   CLEAR                   wVATNOT_E
055300150420     C                   CLEAR                   wVATNOT_I
055400160908     C                   CLEAR                   wVATNOT_J
055500150420     C                   CLEAR                   wVATNOT_S
055600030822     C                   CLEAR                   SkSplitCSV
055700160902     C                   CLEAR                   wFlgCAS
055800030822     C                   EVAL      i = 1
055900030822     C*
056000030822     C* Reimposto i valori di default
056100030822     C                   EXSR      DEFCAM
056200030822     C*
056300010330     C                   ENDSR
056400160902
056500160902
056600010330     C*----------------------------------------------------*
056700030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
056800010330     C*----------------------------------------------------*
056900010330     C     DEFCAM        BEGSR
057000110706     C*
057100160906     C                   SETOFF                                       7172
057200030822     C*
057300030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
057400030715     C* e delimitatore testo.
057500030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
057600170315     C*
057700170315     C* Verifico eventuali valori particolari per il carattere separatore campo
057800170315     C                   IF        CharCSV = '5'
057900170315     C                   EVAL      CharCSV = x'05'
058000170315     C                   ENDIF
058100170315     C*
058200030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
058300030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
058400150420     C                   IF        %subst(vlrppt:5:1) <> *blanks
058500150420     C                   EVAL      pptCSV = %subst(vlrppt:5:1)
058600150420     C                   ELSE
058700150420     C                   EVAL      pptCSV = ';'
058800150420     C                   ENDIF
058900150420     C                   IF        %subst(vlrppt:6:1) <> *blanks
059000150420     C                   EVAL      pptCHR = %subst(vlrppt:6:1)
059100150420     C                   ELSE
059200150420     C                   EVAL      pptCHR = ' '
059300150420     C                   ENDIF
059400090403     C*
059500090403     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
059600160902     C                   EVAL      CharSOS = CharNUM
059700030822     C*
059800030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
059900030715     C                   IF        %subst(vlrppt:1:1) = 'S'
060000160902     C                   SETON                                        50        * OUT => Download
060100030715     C                   ELSE
060200160902     C                   SETOFF                                       50        * OUT => File WW
060300030715     C                   ENDIF
060400030822     C*
060500030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
060600030822     C                   EVAL      posDaDft = 1
060700030822     C                   EVAL      posADft  = 0
060800030822     C                   EVAL      wGiroDft = 0
060900030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
061000030822     C                             posDaDft > 0
061100030822     C*
061200030822     C* Gestisco il 1° giro
061300030822     C                   IF        wGiroDft = 0
061400030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
061500030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
061600030822     C* Incremento il contatore dei "giri"
061700030822     C                   EVAL      wGiroDft = 1
061800030822     C                   ELSE
061900030822     C                   EVAL      posDaDft = posADft
062000030822     C                   ENDIF
062100030822     C* Eseguo lo scan x trovare la fine del campo corrente
062200030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
062300030822     C*
062400030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
062500030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
062600030822     C                   IF        posDaDft > 0 AND
062700030822     C                             posADft  > 0
062800110706     C* RMN
062900110706     C                   IF        %subst(
063000110706     C                             %subst(vlrppt:posDaDft+1:
063100110706     C                             posADft-posDaDft-1):1:3)
063200110706     C                             = 'RMN'
063300110706     C                   IF        %trim(%subst(
063400110706     C                             %subst(vlrppt:posDaDft+1:
063500110706     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063600110706     C                             = '*NSP'
063700160902     C                   SETON                                        71
063800110706     C                   ENDIF
063900110706     C                   ENDIF
064000060331     C* FGS
064100060331     C                   IF        %subst(
064200060331     C                             %subst(vlrppt:posDaDft+1:
064300060331     C                             posADft-posDaDft-1):1:3)
064400060331     C                             = 'FGS'
064500060331     C                   EVAL      PiStr=%trim(%subst(
064600060331     C                             %subst(vlrppt:posDaDft+1:
064700060331     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064800060331     C                   EXSR      CHKNUM
064900060331     C                   IF        PiInt=*on
065000060331     C                   Z-ADD     PiVal         VABFGS
065100060331     C                   ENDIF
065200060331     C                   ENDIF
065300030822     C* NCL
065400030822     C                   IF        %subst(
065500030822     C                             %subst(vlrppt:posDaDft+1:
065600030822     C                             posADft-posDaDft-1):1:3)
065700030822     C                             = 'NCL'
065800030822     C                   EVAL      PiStr=%trim(%subst(
065900030822     C                             %subst(vlrppt:posDaDft+1:
066000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066100030822     C                   EXSR      CHKNUM
066200030822     C                   IF        PiInt=*on
066300030822     C                   Z-ADD     PiVal         VABNCL
066400030822     C                   ENDIF
066500030822     C                   ENDIF
066600030822     C* CCM
066700030822     C                   IF        %subst(
066800030822     C                             %subst(vlrppt:posDaDft+1:
066900030822     C                             posADft-posDaDft-1):1:3)
067000030822     C                             = 'CCM'
067100030822     C                   EVAL      PiStr=%trim(%subst(
067200030822     C                             %subst(vlrppt:posDaDft+1:
067300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067400030822     C                   EXSR      CHKNUM
067500030822     C                   IF        PiInt=*on
067600030822     C                   Z-ADD     PiVal         VABCCM
067700030822     C                   ENDIF
067800030822     C                   ENDIF
067900030822     C* LNP
068000030822     C                   IF        %subst(
068100030822     C                             %subst(vlrppt:posDaDft+1:
068200030822     C                             posADft-posDaDft-1):1:3)
068300030822     C                             = 'LNP'
068400030822     C                   EVAL      PiStr=%trim(%subst(
068500030822     C                             %subst(vlrppt:posDaDft+1:
068600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068700030822     C                   EXSR      CHKNUM
068800030822     C                   IF        PiInt=*on
068900030822     C                   Z-ADD     PiVal         VABLNP
069000030822     C                   ENDIF
069100030822     C                   ENDIF
069200030822     C* NRS
069300030822     C                   IF        %subst(
069400030822     C                             %subst(vlrppt:posDaDft+1:
069500030822     C                             posADft-posDaDft-1):1:3)
069600030822     C                             = 'NRS'
069700030822     C                   EVAL      PiStr=%trim(%subst(
069800030822     C                             %subst(vlrppt:posDaDft+1:
069900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070000030822     C                   EXSR      CHKNUM
070100030822     C                   IF        PiInt=*on
070200030822     C                   Z-ADD     PiVal         VABNRS
070300030822     C                   ENDIF
070400030822     C                   ENDIF
070500160906     C* CTR
070600030822     C                   IF        %subst(
070700030822     C                             %subst(vlrppt:posDaDft+1:
070800030822     C                             posADft-posDaDft-1):1:3)
070900030822     C                             = 'CTR'
071000030822     C                   EVAL      PiStr=%trim(%subst(
071100030822     C                             %subst(vlrppt:posDaDft+1:
071200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071300030822     C                   EXSR      CHKNUM
071400030822     C                   IF        PiInt=*on
071500160906     C                   Z-ADD     PiVal         VABCTR
071600030822     C                   ENDIF
071700030822     C                   ENDIF
071800160906     C* CTE (ovvero CTR per Estero)
071900160906     C                   IF        %subst(
072000160906     C                             %subst(vlrppt:posDaDft+1:
072100160906     C                             posADft-posDaDft-1):1:3)
072200160906     C                             = 'CTE'
072300160906     C                   EVAL      PiStr=%trim(%subst(
072400160906     C                             %subst(vlrppt:posDaDft+1:
072500160906     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072600160906     C                   EXSR      CHKNUM
072700160906     C                   IF        PiInt=*on
072800160906     C                   Z-ADD     PiVal         wVABCTR_E
072900160906     C                   SETON                                        72
073000160906     C                   ENDIF
073100160906     C                   ENDIF
073200030822     C* PKB
073300030822     C                   IF        %subst(
073400030822     C                             %subst(vlrppt:posDaDft+1:
073500030822     C                             posADft-posDaDft-1):1:3)
073600030822     C                             = 'PKB'
073700030822     C                   EVAL      PiStr=%trim(%subst(
073800030822     C                             %subst(vlrppt:posDaDft+1:
073900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074000030822     C                   EXSR      CHKNUM
074100030822     C                   IF        PiNum=*on
074200030822     C                   Z-ADD     PiVal         VABPKB
074300030822     C                   ENDIF
074400030822     C                   ENDIF
074500030822     C* VLB
074600030822     C                   IF        %subst(
074700030822     C                             %subst(vlrppt:posDaDft+1:
074800030822     C                             posADft-posDaDft-1):1:3)
074900030822     C                             = 'VLB'
075000030822     C                   EVAL      PiStr=%trim(%subst(
075100030822     C                             %subst(vlrppt:posDaDft+1:
075200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075300030822     C                   EXSR      CHKNUM
075400030822     C                   IF        PiNum=*on
075500030822     C                   Z-ADD     PiVal         VABVLB
075600030822     C                   ENDIF
075700030822     C                   ENDIF
075800030822     C* QFT
075900030822     C                   IF        %subst(
076000030822     C                             %subst(vlrppt:posDaDft+1:
076100030822     C                             posADft-posDaDft-1):1:3)
076200030822     C                             = 'QFT'
076300030822     C                   EVAL      PiStr=%trim(%subst(
076400030822     C                             %subst(vlrppt:posDaDft+1:
076500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076600030822     C                   EXSR      CHKNUM
076700030822     C                   IF        PiNum=*on
076800030822     C                   Z-ADD     PiVal         VABQFT
076900030822     C                   ENDIF
077000030822     C                   ENDIF
077100170316     C* ATB
077200170316     C                   IF        %subst(
077300170316     C                             %subst(vlrppt:posDaDft+1:
077400170316     C                             posADft-posDaDft-1):1:3)
077500170316     C                             = 'ATB'
077600170316     C                   EVAL      VABATB=%trim(%subst(
077700170316     C                             %subst(vlrppt:posDaDft+1:
077800170316     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077900170316     C                   ENDIF
078000030822     C* CBO
078100030822     C                   IF        %subst(
078200030822     C                             %subst(vlrppt:posDaDft+1:
078300030822     C                             posADft-posDaDft-1):1:3)
078400030822     C                             = 'CBO'
078500030822     C                   EVAL      VABCBO=%trim(%subst(
078600030822     C                             %subst(vlrppt:posDaDft+1:
078700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078800030822     C                   ENDIF
078900030822     C* TSP
079000030822     C                   IF        %subst(
079100030822     C                             %subst(vlrppt:posDaDft+1:
079200030822     C                             posADft-posDaDft-1):1:3)
079300030822     C                             = 'TSP'
079400030822     C                   EVAL      VABTSP=%trim(%subst(
079500030822     C                             %subst(vlrppt:posDaDft+1:
079600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079700030822     C                   ENDIF
079800030822     C* VAS
079900030822     C                   IF        %subst(
080000030822     C                             %subst(vlrppt:posDaDft+1:
080100030822     C                             posADft-posDaDft-1):1:3)
080200030822     C                             = 'VAS'
080300030822     C                   EVAL      VABVAS=%trim(%subst(
080400030822     C                             %subst(vlrppt:posDaDft+1:
080500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080600030822     C                   ENDIF
080700030822     C* VCA
080800030822     C                   IF        %subst(
080900030822     C                             %subst(vlrppt:posDaDft+1:
081000030822     C                             posADft-posDaDft-1):1:3)
081100030822     C                             = 'VCA'
081200030822     C                   EVAL      VABVCA=%trim(%subst(
081300030822     C                             %subst(vlrppt:posDaDft+1:
081400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081500030822     C                   ENDIF
081600030822     C* GCA
081700030822     C                   IF        %subst(
081800030822     C                             %subst(vlrppt:posDaDft+1:
081900030822     C                             posADft-posDaDft-1):1:3)
082000030822     C                             = 'GCA'
082100030822     C                   EVAL      VABGCA=%trim(%subst(
082200030822     C                             %subst(vlrppt:posDaDft+1:
082300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082400030822     C                   ENDIF
082500030822     C* CTM
082600030822     C                   IF        %subst(
082700030822     C                             %subst(vlrppt:posDaDft+1:
082800030822     C                             posADft-posDaDft-1):1:3)
082900030822     C                             = 'CTM'
083000030822     C                   EVAL      VABCTM=%trim(%subst(
083100030822     C                             %subst(vlrppt:posDaDft+1:
083200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
083300030822     C                   ENDIF
083400030822     C* FFD
083500030822     C                   IF        %subst(
083600030822     C                             %subst(vlrppt:posDaDft+1:
083700030822     C                             posADft-posDaDft-1):1:3)
083800030822     C                             = 'FFD'
083900030822     C                   EVAL      VABFFD=%trim(%subst(
084000030822     C                             %subst(vlrppt:posDaDft+1:
084100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
084200030822     C                   ENDIF
084300030822     C* VAD
084400030822     C                   IF        %subst(
084500030822     C                             %subst(vlrppt:posDaDft+1:
084600030822     C                             posADft-posDaDft-1):1:3)
084700030822     C                             = 'VAD'
084800030822     C                   EVAL      VABVAD=%trim(%subst(
084900030822     C                             %subst(vlrppt:posDaDft+1:
085000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
085100030822     C                   ENDIF
085200030822     C* GMA
085300030822     C                   IF        %subst(
085400030822     C                             %subst(vlrppt:posDaDft+1:
085500030822     C                             posADft-posDaDft-1):1:3)
085600030822     C                             = 'GMA'
085700030822     C                   EVAL      VABGMA=%trim(%subst(
085800030822     C                             %subst(vlrppt:posDaDft+1:
085900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
086000030822     C                   ENDIF
086100030822     C* GGA
086200030822     C                   IF        %subst(
086300030822     C                             %subst(vlrppt:posDaDft+1:
086400030822     C                             posADft-posDaDft-1):1:3)
086500030822     C                             = 'GGA'
086600030822     C                   EVAL      VABGGA=%trim(%subst(
086700030822     C                             %subst(vlrppt:posDaDft+1:
086800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
086900030822     C                   ENDIF
087000030822     C* GVA
087100030822     C                   IF        %subst(
087200030822     C                             %subst(vlrppt:posDaDft+1:
087300030822     C                             posADft-posDaDft-1):1:3)
087400030822     C                             = 'GVA'
087500030822     C                   EVAL      VABGVA=%trim(%subst(
087600030822     C                             %subst(vlrppt:posDaDft+1:
087700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
087800030822     C                   ENDIF
087900030822     C* TC1
088000030822     C                   IF        %subst(
088100030822     C                             %subst(vlrppt:posDaDft+1:
088200030822     C                             posADft-posDaDft-1):1:3)
088300030822     C                             = 'TC1'
088400030822     C                   EVAL      VABTC1=%trim(%subst(
088500030822     C                             %subst(vlrppt:posDaDft+1:
088600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
088700030822     C                   ENDIF
088800030822     C* TC2
088900030822     C                   IF        %subst(
089000030822     C                             %subst(vlrppt:posDaDft+1:
089100030822     C                             posADft-posDaDft-1):1:3)
089200030822     C                             = 'TC2'
089300030822     C                   EVAL      VABTC2=%trim(%subst(
089400030822     C                             %subst(vlrppt:posDaDft+1:
089500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
089600030822     C                   ENDIF
089700030822     C* ...
089800030822     C                   ENDIF
089900030822     C                   ENDDO
090000020204     C*
090100010330     C                   ENDSR
090200010607     C*----------------------------------------------------*
090300021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
090400010607     C*----------------------------------------------------*
090500010607     C     IMPVAB        BEGSR
090600010607     C*
090700010607     C                   SETOFF                                       3132
090800030515     C*
090900010607     C                   MOVEL     datcor        VABAAS
091000010607     C                   MOVE      datcor        VABMGS
091100160902     C*
091200160902     C* Dopo aver splittato nei vari campi eseguo 1 prima "normalizzazione" x eliminare
091300160902     C* i caratteri di delimitazione testo e gli apici
091400160902     C                   EVAL      SkSplitCSV = SkSplitCSV_5
091500160902     C                   EVAL      i = 1
091600160902     C                   DOW       i < %elem(SkSplitCSV)
091700160902     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
091800160902     C     '''':' '      XLATE     SkSplitCSV(i) SkSplitCSV(i)
091900160902     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
092000160902     C                   EVAL      i = i + 1
092100160902     C                   ENDDO
092200010607     C*
092300030715     C**********
092400160902     C* Valorizzo la lista delle INTESTAZIONI colonne cliente
092500030715     C**********
092600030715     C                   IF        wGiro = *zeros
092700160902     C*
092800160902     C                   EVAL      wGiro = 1
092900160902     C                   EVAL      SkSplitFLD = SkSplitCSV
093000160902     C*
093100160902     C                   ELSE
093200160902     C*
093300160902     C**********
093400160902     C* Elaboro le righe di DATI cliente
093500160902     C**********
093600160902     C                   EVAL      wGiro = 2
093700030715     C*
093800030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
093900030715     C                   EVAL      i = 1
094000030715     C                   DOW       i < %elem(SkSplitFLD)
094100030715     C*
094200030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
094300030715     C                   IF        SkSplitFLD(i) <> *blanks AND
094400030715     C                             SkSplitCSV(i) <> *blanks
094500130624     C*** ATB
094600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
094700150420     C                   EVAL      VABATB = SkSplitCSV(i)
094800030721     C                   ENDIF
094900030715     C*** RSD
095000160902     C                   IF        %trim(SkSplitFLD(i)) =
095100170522     C                             'shipment_address_name'
095200030715     C                   EVAL      VABRSD = SkSplitCSV(i)
095300030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
095400030715     C     '@':'A'       XLATE     VABRSD        VABRSD
095500030715     C* ==
095600030715     C                   ENDIF
095700030721     C*** RD2
095800160902     C                   IF        %trim(SkSplitFLD(i)) =
095900170522     C                             'shipment_address_street_2'
096000160902     C                   EVAL      VABRD2 = SkSplitCSV(i)
096100160902     C                   ENDIF
096200030715     C*** IND
096300160902     C                   IF        %trim(SkSplitFLD(i)) =
096400170522     C                             'shipment_address_street'
096500030715     C                   EVAL      VABIND = SkSplitCSV(i)
096600030715     C                   ENDIF
096700030715     C*** LOD
096800160902     C                   IF        %trim(SkSplitFLD(i)) =
096900170522     C                             'shipment_address_city'
097000030715     C                   EVAL      VABLOD = SkSplitCSV(i)
097100030715     C                   ENDIF
097200030715     C*** PRD
097300160902     C                   IF        %trim(SkSplitFLD(i)) =
097400170522     C                             'shipment_address_stat'
097500030715     C                   EVAL      VABPRD = SkSplitCSV(i)
097600030715     C                   ENDIF
097700160902     C*** CAD
097800160902     C                   IF        %trim(SkSplitFLD(i)) =
097900170522     C                             'shipment_address_postal_code'
098000160902     C                   EVAL      VABCAD = SkSplitCSV(i)
098100160902     C                   ENDIF
098200030721     C*** NZD
098300160902     C                   IF        %trim(SkSplitFLD(i)) =
098400170522     C                             'shipment_address_country'
098500170315     C                   EVAL      VABNZD = SkSplitCSV(i)
098600170316     C                   Z-ADD     1             jNAZ
098700170316     C     VABNZD        LOOKUP    skNAZISO(jNAZ)                         13
098800170316     C                   IF        %found
098900170316     C                   EVAL      VABNZD = skNAZBAR(jNAZ)
099000170316     C                   ENDIF
099100030721     C                   ENDIF
099200170522     C*** RMA
099300170522     C                   IF        %trim(SkSplitFLD(i)) = 'groupon_number'
099400170522     C                   EVAL      VABRMA = SkSplitCSV(i)
099500150515     C                   ENDIF
099600030715     C*** NOT
099700170522     C                   IF        %trim(SkSplitFLD(i)) =
099800170522     C                             'shipment_address_street_2'
099900030715     C                   EVAL      VABNOT = SkSplitCSV(i)
100000030715     C                   ENDIF
100100030715     C*** NT2
100200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
100300030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
100400030715     C                   ENDIF
100500030715     C*** VCA
100600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
100700030715     C                   EVAL      VABVCA = SkSplitCSV(i)
100800030715     C                   ENDIF
100900030715     C*** VAS
101000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
101100030715     C                   EVAL      VABVAS = SkSplitCSV(i)
101200030715     C                   ENDIF
101300030715     C*** FFD
101400160902     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
101500030715     C                   EVAL      VABFFD = SkSplitCSV(i)
101600030715     C                   ENDIF
101700160902     C*** CBO
101800160902     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
101900160902     C                   EVAL      VABCBO = SkSplitCSV(i)
102000150420     C                   ENDIF
102100030721     C*** GC1
102200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
102300150420     C                   IF        %len(%trim(SkSplitCSV(i))) = 1
102400150420     C                   MOVEL     *blanks       wGC               1
102500150420     C                   EVAL      wGC = %trim(SkSplitCSV(i))
102600150420     C                   IF        SkSplitCSV(i) = 'M' OR
102700150420     C                             SkSplitCSV(i) = 'P'
102800150420     C                   MOVE(P)   wGC           VABGC1
102900150420     C                   ELSE
103000150420     C                   MOVEL(P)  wGC           VABGC1
103100150420     C                   ENDIF
103200150420     C                   ENDIF
103300030721     C                   ENDIF
103400030721     C*** GC2
103500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
103600120314     C                   IF        %len(%trim(SkSplitCSV(i))) = 1
103700120314     C                   MOVEL     *blanks       wGC               1
103800120314     C                   EVAL      wGC = %trim(SkSplitCSV(i))
103900120314     C                   IF        SkSplitCSV(i) = 'M' OR
104000120314     C                             SkSplitCSV(i) = 'P'
104100120314     C                   MOVE(P)   wGC           VABGC2
104200120314     C                   ELSE
104300120314     C                   MOVEL(P)  wGC           VABGC2
104400120314     C                   ENDIF
104500120314     C                   ENDIF
104600030721     C                   ENDIF
104700030721     C*** TSP
104800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
104900030721     C                   EVAL      VABTSP = SkSplitCSV(i)
105000030721     C                   ENDIF
105100030721     C*** NAS
105200170522     C                   IF        %trim(SkSplitFLD(i)) =
105300170522     C                             'merchant_sku_item'
105400030721     C                   EVAL      VABNAS = SkSplitCSV(i)
105500030721     C                   ENDIF
105600030721     C*** TIC
105700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
105800030721     C                   EVAL      VABTIC = SkSplitCSV(i)
105900030721     C                   ENDIF
106000030721     C*** GCA
106100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
106200030721     C                   EVAL      VABGCA = SkSplitCSV(i)
106300030721     C                   ENDIF
106400030721     C*** XCO
106500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
106600030721     C                   EVAL      VABXCO = SkSplitCSV(i)
106700030721     C                   ENDIF
106800030721     C*** CTM
106900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
107000030721     C                   EVAL      VABCTM = SkSplitCSV(i)
107100030721     C                   ENDIF
107200030721     C*** TCR
107300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
107400030721     C                   EVAL      VABTCR = SkSplitCSV(i)
107500030721     C                   ENDIF
107600030721     C*** CTS
107700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
107800030721     C                   EVAL      VABCTS = SkSplitCSV(i)
107900030721     C                   ENDIF
108000030721     C*** FTM
108100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
108200030721     C                   EVAL      VABFTM = SkSplitCSV(i)
108300030721     C                   ENDIF
108400030721     C*** VAD
108500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
108600030721     C                   EVAL      VABVAD = SkSplitCSV(i)
108700030721     C                   ENDIF
108800030721     C*** GMA
108900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
109000030721     C                   EVAL      VABGMA = SkSplitCSV(i)
109100030721     C                   ENDIF
109200030721     C*** GGA
109300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
109400030721     C                   EVAL      VABGGA = SkSplitCSV(i)
109500030721     C                   ENDIF
109600030721     C*** GVA
109700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
109800030721     C                   EVAL      VABGVA = SkSplitCSV(i)
109900030721     C                   ENDIF
110000030721     C*** TC1
110100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
110200030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
110300030721     C                   ENDIF
110400030721     C*** TC2
110500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
110600030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
110700030721     C                   ENDIF
110800030721     C*** SCL
110900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
111000030721     C                   EVAL      VABSCL = SkSplitCSV(i)
111100030721     C                   ENDIF
111200030721     C*** RMO
111300160902     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
111400030721     C                   EVAL      VABRMO = SkSplitCSV(i)
111500030721     C                   ENDIF
111600160902     C*** CMO
111700160902     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
111800160902     C                   EVAL      VABCMO = SkSplitCSV(i)
111900160902     C                   ENDIF
112000030721     C*** NMO
112100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
112200030721     C                   EVAL      VABNMO = SkSplitCSV(i)
112300030721     C                   ENDIF
112400160908     C*** NOT_A
112500160908     C                   IF        %trim(SkSplitFLD(i)) =
112600170522     C                             'billing_address_name'
112700160908     C                   EVAL      wVATNOT_A = SkSplitCSV(i)
112800160908     C                   ENDIF
112900160902     C*** NOT_B
113000160902     C                   IF        %trim(SkSplitFLD(i)) =
113100170522     C                             'customer_phone'
113200160902     C                   EVAL      wVATNOT_B = SkSplitCSV(i)
113300160902     C                   ENDIF
113400150420     C*** NOT_I
113500170522     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
113600150420     C                   EVAL      wVATNOT_I = SkSplitCSV(i)
113700150420     C                   ENDIF
113800150420     C*** NOT_S
113900170315     C                   IF        %trim(SkSplitFLD(i)) =
114000170522     C                             'customer_phone'
114100150420     C                   EVAL      wVATNOT_S = SkSplitCSV(i)
114200150420     C                   ENDIF
114300030721     C*
114400070705     C* Reperisco quindi i campi numerici...
114500030721     C*** RMN
114600160908     C                   IF        %trim(SkSplitFLD(i)) =
114700170522     C                             'fulfillment_line_item_id'
114800030721     C                   EVAL      PiStr=SkSplitCSV(i)
114900030721     C                   EXSR      CHKNUM
115000030721     C                   IF        PiInt=*on
115100030721     C                   Z-ADD     PiVal         VABRMN
115200030721     C                   ELSE
115300030721     C                   SETON                                        32
115400030721     C                   EVAL      VABRMN = 1
115500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115600030721     C                             + ' ' + 'VABRMN'
115700030721     C                   ENDIF
115800030721     C                   ENDIF
115900030721     C*** NSP
116000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
116100030721     C                   EVAL      PiStr=SkSplitCSV(i)
116200030721     C                   EXSR      CHKNUM
116300100517     C                   IF        PiInt=*on
116400030721     C                   Z-ADD     PiVal         VABNSP
116500030721     C                   ELSE
116600030721     C                   SETON                                        32
116700030721     C                   EVAL      VABNSP = *zeros
116800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116900030721     C                             + ' ' + 'VABNSP'
117000030721     C                   ENDIF
117100030721     C                   ENDIF
117200160902     C*** NCL
117300160902     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
117400030721     C                   EVAL      PiStr=SkSplitCSV(i)
117500030721     C                   EXSR      CHKNUM
117600030721     C                   IF        PiInt=*on
117700030721     C                   Z-ADD     PiVal         VABNCL
117800030721     C                   ELSE
117900030721     C                   SETON                                        32
118000030721     C                   EVAL      VABNCL = *zeros
118100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118200030721     C                             + ' ' + 'VABNCL'
118300030721     C                   ENDIF
118400030721     C                   ENDIF
118500030721     C*** PKB
118600160902     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
118700030721     C                   EVAL      PiStr=SkSplitCSV(i)
118800030721     C                   EXSR      CHKNUM
118900030721     C                   IF        PiNum=*on
119000030721     C                   Z-ADD     PiVal         VABPKB
119100030721     C                   ELSE
119200030721     C                   SETON                                        32
119300030721     C                   EVAL      VABPKB = *zeros
119400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119500030721     C                             + ' ' + 'VABPKB'
119600030721     C                   ENDIF
119700030721     C                   ENDIF
119800160908     C*** CAS
119900160908     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
120000160908     C                   IF        SkSplitCSV(i) <> *blanks
120100160908     C                   EVAL      wFlgCAS = 1
120200160908     C                   ENDIF
120300160902     C                   EVAL      PiStr=SkSplitCSV(i)
120400160902     C                   EXSR      CHKNUM
120500160902     C                   IF        PiNum=*on
120600160902     C                   Z-ADD     PiVal         VABCAS
120700160902     C                   MOVEL     'EUR'         VABVCA
120800160902     C                   ELSE
120900160902     C                   SETON                                        32
121000160902     C                   EVAL      VABCAS = *zeros
121100160902     C                   EVAL      vinmsg = %trimr(vinmsg)
121200160902     C                             + ' ' + 'VABCAS'
121300160902     C                   ENDIF
121400160902     C                   ENDIF
121500030721     C*** CCM
121600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
121700030721     C                   EVAL      PiStr=SkSplitCSV(i)
121800030721     C                   EXSR      CHKNUM
121900030721     C                   IF        PiInt=*on      AND
122000030721     C                             PiVal<=9999999 AND
122100030721     C                             PiVal>*zeros
122200030721     C                   Z-ADD     PiVal         VABCCM
122300030721     C                   ELSE
122400030721     C                   SETON                                        32
122500030721     C                   EVAL      VABCCM = *zeros
122600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122700030721     C                             + ' ' + 'VABCCM'
122800030721     C                   ENDIF
122900030721     C                   ENDIF
123000030721     C*** LNP
123100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
123200030721     C                   EVAL      PiStr=SkSplitCSV(i)
123300030721     C                   EXSR      CHKNUM
123400030721     C                   IF        PiInt=*on    AND
123500030721     C                             PiVal<=999   AND
123600030721     C                             PiVal>*zeros
123700030721     C                   Z-ADD     PiVal         VABLNP
123800030721     C                   ELSE
123900030721     C                   SETON                                        32
124000030721     C                   EVAL      VABLNP = *zeros
124100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124200030721     C                             + ' ' + 'VABLNP'
124300030721     C                   ENDIF
124400030721     C                   ENDIF
124500030721     C*** NRS
124600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
124700030721     C                   EVAL      PiStr=SkSplitCSV(i)
124800030721     C                   EXSR      CHKNUM
124900030721     C                   IF        PiInt=*on AND
125000030721     C                             PiVal<=99
125100030721     C                   Z-ADD     PiVal         VABNRS
125200030721     C                   ELSE
125300030721     C                   SETON                                        32
125400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125500030721     C                             + ' ' + 'VABNRS'
125600030721     C                   ENDIF
125700030721     C                   ENDIF
125800030721     C*** LNA
125900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
126000120314     C                   IF        %trim(SkSplitCSV(i)) <> *zeros  AND
126100120314     C                             %trim(SkSplitCSV(i)) <> '0'     AND
126200120314     C                             %trim(SkSplitCSV(i)) <> '000'   AND
126300120314     C                             %trim(SkSplitCSV(i)) <> *blanks
126400030721     C                   EVAL      PiStr=SkSplitCSV(i)
126500030721     C                   EXSR      CHKNUM
126600030721     C                   IF        PiInt=*on    AND
126700030721     C                             PiVal<=999   AND
126800030721     C                             PiVal>*zeros
126900030721     C                   Z-ADD     PiVal         VABLNA
127000030721     C                   ELSE
127100030721     C                   SETON                                        32
127200030721     C                   EVAL      VABLNA = *zeros
127300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127400030721     C                             + ' ' + 'VABLNA'
127500120314     C                   ENDIF
127600030721     C                   ENDIF
127700030721     C                   ENDIF
127800030721     C*** CTR
127900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
128000030721     C                   EVAL      PiStr=SkSplitCSV(i)
128100030721     C                   EXSR      CHKNUM
128200030721     C                   IF        PiInt=*on  AND
128300030721     C                             PiVal<=999
128400030721     C                   Z-ADD     PiVal         VABCTR
128500030721     C                   ELSE
128600030721     C                   SETON                                        32
128700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128800030721     C                             + ' ' + 'VABCTR'
128900030721     C                   ENDIF
129000030721     C                   ENDIF
129100030721     C*** IAS
129200160902     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
129300030721     C                   EVAL      PiStr=SkSplitCSV(i)
129400030721     C                   EXSR      CHKNUM
129500030721     C                   IF        PiNum=*on
129600030721     C                   Z-ADD     PiVal         VABIAS
129700030721     C                   ELSE
129800030721     C                   SETON                                        32
129900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130000030721     C                             + ' ' + 'VABIAS'
130100030721     C                   ENDIF
130200030721     C                   ENDIF
130300030721     C*** VLB
130400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
130500030721     C                   EVAL      PiStr=SkSplitCSV(i)
130600030721     C                   EXSR      CHKNUM
130700030721     C                   IF        PiNum=*on
130800030721     C                   Z-ADD     PiVal         VABVLB
130900030721     C                   ELSE
131000030721     C                   SETON                                        32
131100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131200030721     C                             + ' ' + 'VABVLB'
131300030721     C                   ENDIF
131400030721     C                   ENDIF
131500030721     C*** QFT
131600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
131700030721     C                   EVAL      PiStr=SkSplitCSV(i)
131800030721     C                   EXSR      CHKNUM
131900030721     C                   IF        PiNum=*on
132000030721     C                   Z-ADD     PiVal         VABQFT
132100030721     C                   ELSE
132200030721     C                   SETON                                        32
132300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132400030721     C                             + ' ' + 'VABQFT'
132500030721     C                   ENDIF
132600030721     C                   ENDIF
132700030721     C*** NCD
132800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
132900030721     C                   EVAL      PiStr=SkSplitCSV(i)
133000030721     C                   EXSR      CHKNUM
133100030721     C                   IF        PiInt=*on      AND
133200030721     C                             PiVal<=9999999
133300030721     C                   Z-ADD     PiVal         VABNCD
133400030721     C                   ELSE
133500030721     C                   SETON                                        32
133600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133700030721     C                             + ' ' + 'VABNCD'
133800030721     C                   ENDIF
133900030721     C                   ENDIF
134000030721     C*** NCA
134100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
134200030721     C                   EVAL      PiStr=SkSplitCSV(i)
134300030721     C                   EXSR      CHKNUM
134400030721     C                   IF        PiInt=*on      AND
134500030721     C                             PiVal<=9999999
134600030721     C                   Z-ADD     PiVal         VABNCA
134700030721     C                   ELSE
134800030721     C                   SETON                                        32
134900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135000030721     C                             + ' ' + 'VABNCA'
135100030721     C                   ENDIF
135200030721     C                   ENDIF
135300030721     C*** ZNC
135400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
135500030721     C                   EVAL      PiStr=SkSplitCSV(i)
135600030721     C                   EXSR      CHKNUM
135700030721     C                   IF        PiInt=*on AND
135800030721     C                             PiVal<=99
135900030721     C                   Z-ADD     PiVal         VABZNC
136000030721     C                   ELSE
136100030721     C                   SETON                                        32
136200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136300030721     C                             + ' ' + 'VABZNC'
136400030721     C                   ENDIF
136500030721     C                   ENDIF
136600030721     C*** DCR
136700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
136800030721     C                   EVAL      PiStr=SkSplitCSV(i)
136900030721     C                   EXSR      CHKNUM
137000030721     C                   IF        PiInt=*on       AND
137100030721     C                             PiVal<=99999999
137200030721     C                   Z-ADD     PiVal         VABDCR
137300030721     C                   ELSE
137400030721     C                   SETON                                        32
137500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137600030721     C                             + ' ' + 'VABDCR'
137700030721     C                   ENDIF
137800030721     C                   ENDIF
137900030721     C*** HCR
138000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
138100030721     C                   EVAL      PiStr=SkSplitCSV(i)
138200030721     C                   EXSR      CHKNUM
138300030721     C                   IF        PiInt=*on   AND
138400030721     C                             PiVal<=9999
138500030721     C                   Z-ADD     PiVal         VABHCR
138600030721     C                   ELSE
138700030721     C                   SETON                                        32
138800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138900030721     C                             + ' ' + 'VABHCR'
139000030721     C                   ENDIF
139100030721     C                   ENDIF
139200030721     C*** VMD
139300160902     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
139400030721     C                   EVAL      PiStr=SkSplitCSV(i)
139500030721     C                   EXSR      CHKNUM
139600030721     C                   IF        PiNum=*on
139700030721     C                   Z-ADD     PiVal         VABVMD
139800030721     C                   ELSE
139900030721     C                   SETON                                        32
140000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
140100030721     C                             + ' ' + 'VABVMD'
140200030721     C                   ENDIF
140300030721     C                   ENDIF
140400030721     C*** ANT
140500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
140600030721     C                   EVAL      PiStr=SkSplitCSV(i)
140700030721     C                   EXSR      CHKNUM
140800030721     C                   IF        PiInt=*on        AND
140900030721     C                             PiVal<=999999999
141000030721     C                   Z-ADD     PiVal         VABANT
141100030721     C                   ELSE
141200030721     C                   SETON                                        32
141300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
141400030721     C                             + ' ' + 'VABANT'
141500030721     C                   ENDIF
141600030721     C                   ENDIF
141700030715     C*
141800030715     C                   ENDIF
141900030715     C                   EVAL      i = i + 1
142000030715     C                   ENDDO
142100070705     C*
142200160902     C* Considerazioni particolari "fuori ciclo"
142300110706     C*
142400160908     C* Verifica campi "particolari" VAB
142500160908     C                   EXSR      CHKVAB
142600160908     C*
142700030715     C                   ENDIF
142800020204     C*
142900010607     C                   ENDSR
143000010607     C*----------------------------------------------------*
143100160908
143200160908
143300160908     C*----------------------------------------------------*
143400160908     C*  CONSIDERAZIONI SU CAMPI PARTICOLARI VAB
143500160908     C*----------------------------------------------------*
143600160908     C     CHKVAB        BEGSR
143700160908     C*
143800160908     C*** RMN
143900160908     C                   IF        *in71
144000160908     C                   EVAL      VABRMN = VABNSP
144100160908     C                   ENDIF
144200160908     C                   IF        VABRMN = *zeros
144300160908     C                   EVAL      VABRMN = 1
144400160908     C                   ENDIF
144500160908     C*** NZD
144600160908     C                   IF        VABNZD = 'IT'  OR
144700160908     C                             VABNZD = 'ITA' OR
144800160908     C                             VABNZD = 'Ita' OR
144900160908     C                             VABNZD = 'ita'
145000160908     C                   EVAL      VABNZD = *blanks
145100160908     C                   ENDIF
145200160908     C*** CTR
145300160908     C                   IF        *in72
145400160908     C                   IF        VABNZD <> *blanks
145500160908     C                   EVAL      VABCTR =  wVABCTR_E
145600160908     C                   ENDIF
145700160908     C                   ENDIF
145800160908     C*** CBO
145900160908     C                   IF        VABCBO = *blanks
146000160908     C                   EVAL      VABCBO = '1'
146100160908     C                   ENDIF
146200160908     C*** NCL
146300160908     C                   IF        VABNCL = *zeros
146400160908     C                   EVAL      VABNCL = 1
146500160908     C                   ENDIF
146600160908     C*** CCM
146700160908     C                   IF        VABCCM = *zeros
146800160908     C                   EVAL      PiStr=VLRKSC
146900160908     C                   EXSR      CHKNUM
147000160908     C                   IF        PiInt=*on
147100160908     C                   Z-ADD     PiVal         VABCCM
147200160908     C                   ELSE
147300160908     C                   SETON                                        31
147400160908     C                   ENDIF
147500160908     C                   ENDIF
147600160908     C*** LNP
147700160908     C                   IF        VABLNP = *zeros
147800160908     C                   IF        VLRPOI = 046 OR
147900160908     C                             VLRPOI = 789 OR
148000160908     C                             VLRPOI = 999
148100160908     C                   MOVEL     VABCCM        VABLNP
148200160908     C                   ELSE
148300160908     C                   EVAL      VABLNP = VLRPOI
148400160908     C                   ENDIF
148500160908     C                   ENDIF
148600160908     C*** LNP
148700160908     C                   IF        VABFGS = *zeros
148800160908     C                   EVAL      VABFGS = VABLNP
148900160908     C                   ENDIF
149000160908     C*** FGS / LNP
149100160908     C                   IF        VABFGS = *zeros OR
149200160908     C                             VABLNP = *zeros
149300160908     C                   SETON                                        31
149400160908     C                   ENDIF
149500160908     C*** CAS
149600160908     C                   IF        wFlgCAS = *zeros
149700160908     C                   EVAL      VABCAS = *zeros
149800160908     C                   EVAL      VABVCA = *blanks
149900160908     C                   ENDIF
150000160908     C*** NOT
150100160908     C                   IF        VABNOT = *blanks AND wVABNOT <> *blanks
150200160908     C                   EVAL      VABNOT = wVABNOT
150300160908     C                   ENDIF
150400160908     C*
150500160908     C* Se provincia nn valorizzata la reperisco
150600160908     C* tramite TISI95R a seconda dei dati d instradamento presenti
150700170316     C                   IF        VABNZD = *blanks AND
150800170316     C                             VABPRD = *blanks
150900160908     C                   CLEAR                   TISI95DS
151000160908     C                   EVAL      I95TCN = '3'
151100160908     C                   Z-ADD     datcor        I95DAT
151200160908     C                   EVAL      I95NAR = VABNZD
151300160908     C                   EVAL      I95CAP = VABCAD
151400160908     C                   EVAL      I95LOC = VABLOD
151500160908     C                   CALL      'TISI95R'
151600160908     C                   PARM                    TISI95DS
151700160908     C                   EVAL      VABPRD = O95PRV
151800160908     C                   ENDIF
151900160908     C*
152000160908     C* Eseguo routine finale x considerazioni specifiche su importi/divise
152100160908     C                   EXSR      CHKIMPDIV
152200160908     C*
152300160908     C                   ENDSR
152400160908     C***
152500010601
152600020204
152700020204     C*----------------------------------------------------*
152800020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
152900020204     C*----------------------------------------------------*
153000020204     C     CHKIMPDIV     BEGSR
153100060524     C*
153200060524     C* Verifico il codice bolla in base al tipo porto e alla presenza o meno del contrassegno
153300160906     C                   IF        vabCAS  > *zeros OR
153400160906     C                             wFlgCAS > *zeros
153500060524     C                   IF        vabCBO = '1'
153600060524     C                   EVAL      vabCBO = '4'
153700060524     C                   ENDIF
153800060524     C                   IF        vabCBO = '2'
153900060524     C                   EVAL      vabCBO = '6'
154000060524     C                   ENDIF
154100060524     C                   ENDIF
154200020204     C*
154300020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
154400020204     C                   Z-ADD     *zeros        wrkDec            9 9
154500020204     C*
154600020204     C* Come prima cosa effettuo considerazioni sulla divisa
154700020204     C                   IF        vabIAS > *zeros
154800150420     C                   IF        vabVAS =  *blanks
154900150420     C                   EVAL      vabVAS =  'EUR'
155000020204     C                   ENDIF
155100020204     C                   ENDIF
155200020204     C*
155300020204     C                   IF        vabCAS > *zeros
155400150420     C                   IF        vabVCA <> *blanks
155500150420     C                   EVAL      vabVCA =  'EUR'
155600020204     C                   ENDIF
155700020204     C                   ENDIF
155800020204     C*
155900020204     C                   IF        vabVMD > *zeros
156000150420     C                   IF        vabVAD <> *blanks
156100150420     C                   EVAL      vabVAD =  'EUR'
156200020204     C                   ENDIF
156300020204     C                   ENDIF
156400020204     C*
156500020204     C                   ENDSR
156600020204     C***
156700010330
156800010330
156900010330     C*----------------------------------------------------*
157000010330     C*  CONTROLLO NUMERICITA' CAMPI
157100010330     C*----------------------------------------------------*
157200010330     C     CHKNUM        BEGSR
157300010330     C*
157400010606     C                   IF        PiDecChr = *blanks
157500030715     C                   EVAL      PiDecChr = CharNUM
157600010606     C                   ENDIF
157700010606     C*
157800010606     C                   CALL(e)   'ISNUMERIC'
157900010330     C                   PARM                    PiStr            30
158000010606     C                   PARM                    PiDecChr          1
158100010330     C                   PARM      *ZEROS        PiVal            30 9
158200010330     C                   PARM      '0'           PiInt             1
158300010330     C                   PARM      '0'           PiNum             1
158400010330     C                   IF        %error
158500010606     C                   EVAL      PiNum=*off
158600010330     C                   ENDIF
158700010330     C*
158800010330     C                   ENDSR
158900010330     C***
159000160906
159100160906
159200160906      /TITLE Valorizzazione Progressivo Applicazione
159300160906     C     calprog       begsr
159400160906     C*
159500160906     C                   movel     *blanks       dwlisv            2
159600160906     C                   movel     *all'0'       dwlprg           10
159700160906     C                   z-add     *zeros        wrkprg            8 0
159800160906     C*
159900160906     C                   eval      dwlisv = vlrisv
160000160906     C*
160100170324     C     *start        setll     tis7prgf
160200160906     C                   read(e)   tis7prgf
160300160906     C                   if        not %error
160400160906     C                   eval      dwlprg = f_tis7prgf
160500160906     C*
160600160906     C                   move(p)   dwlprg        wrkprg
160700160906     C                   add       1             wrkprg
160800160906     C                   move(p)   wrkprg        dwlprg
160900160906     C                   movel     dwlisv        dwlprg
161000160906     C*
161100160906     C                   eval      f_tis7prgf = dwlprg
161200160906     C                   update    tis7prg0
161300170324     C                   else
161400170324     C                   eval      f_tis7prgf = 'XX00000000'
161500160906     C                   endif
161600160906     C*
161700160906     C                   endsr
161800050421
161900160908
162000160908     C     *pssr         BEGSR
162100160908     C*
162200160908     C                   if        %open(tivin00r)
162300160908     C                   close     tivin00r
162400160908     C                   endif
162500160908     C                   if        %open(fivabwwr)
162600160908     C                   close     fivabwwr
162700160908     C                   endif
162800160908     C                   if        %open(fivatwwr)
162900160908     C                   close     fivatwwr
163000160908     C                   endif
163100160908     C*
163200160908     C* Effettuo la chiamata al CLLE preposto
163300160908     C  N50              call(e)   'TITVVTC'
163400160908     C                   parm                    parccm
163500160908     C                   parm                    parmbr
163600160908     C                   parm      '2'           paropz
163700160908     C*
163800160908     C                   eval      wrkesito = '2'
163900160908     C*
164000160908     C                   seton                                        LR
164100160908     C*
164200160908     C                   ENDSR     '*CANCL'
164300160908     C***
164400100419
164500100419
164600160902     C     normCSV       BEGSR
164700160902     C*
164800160902     C                   z-add     *zeros        wLenVINDTA        4 0
164900160902     C                   z-add     *zeros        wFlgVINDTA        1 0
165000160902     C                   setoff                                       80
165100160902     C*
165200160902     C                   dow       wLenVINDTA < %len(vindta)
165300160902     C                   eval      wLenVINDTA = wLenVINDTA + 1
165400160902     C                   movel     *blanks       wAlfa1            1
165500160902     C                   eval      wAlfa1 = %subst(vindta:wLenVINDTA:1)
165600160902     C*
165700160902     C* Se byte inferiore a HEX40 è un byte "non stampabile" per cui considero *blank
165800170315     C                   if        wAlfa1 <  X'40' AND
165900170315     C                             wAlfa1 <> CharCSV
166000160902     C                   eval      %subst(vindta:wLenVINDTA:1) = *blanks
166100160902     C                   endif
166200160902     C*
166300160902     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
166400160902     C                   if        wAlfa1 <> CharCSV and
166500160902     C                             wAlfa1 <> CharTXT and
166600160902     C                             wAlfa1 <> CharNUM
166700160902     C                   z-add     1             wFlgVINDTA
166800160902     C                   endif
166900160902     C*
167000160902     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
167100160902     C* separatori campi
167200160902     C                   if        CharTXT <> *blanks
167300160902     C                   if        wAlfa1 = CharTXT
167400160902     C                   if        not *in80
167500160902     C                   seton                                        80
167600160902     C                   else
167700160902     C                   setoff                                       80
167800160902     C                   endif
167900160902     C                   endif
168000160902     C                   if        wAlfa1 = CharCSV and *in80 = *on
168100160902     C                   eval      %subst(vindta:wLenVINDTA:1) = CharSOS
168200160902     C                   endif
168300160902     C                   endif
168400160902     C*
168500160902     C                   enddo
168600100419     C*
168700100419     C                   ENDSR
168800100419     C***
168900150420
169000150420
169100150420      /TITLE Scrittura record in file TIVGD00F (file VAS generico download)
169200160902     C     wriVAB_VGD    BEGSR
169300160909     C*
169400160909     C* Verifica campi "particolari" VAB
169500160909     C                   exsr      repNSP
169600160909     C*
169700160909     C                   setoff                                       45
169800160909     C                   if        depSPE  = *zeros OR
169900160909     C                             depSPE <> vabNSP
170000160909     C                   seton                                        45
170100160909     C                   eval      depSPE  = vabNSP
170200160909     C                   endif
170300160909     C*
170400160909     C                   if        not *in31
170500150420     C*
170600160902     C* Se primo giro allora scrivo testata
170700160902     C                   if        wVGD_Head = *zeros
170800160902     C                   eval      wVGD_Head = 1
170900160902     C                   eval      wBufferOut =
171000170316     C                             %trim(pptCHR)+'VABATB' +%trim(pptCHR)+pptCSV+
171100160902     C                             %trim(pptCHR)+'VABCCM' +%trim(pptCHR)+pptCSV+
171200160902     C                             %trim(pptCHR)+'VABLNP' +%trim(pptCHR)+pptCSV+
171300160902     C                             %trim(pptCHR)+'VABCTR' +%trim(pptCHR)+pptCSV+
171400160902     C                             %trim(pptCHR)+'VABRSD' +%trim(pptCHR)+pptCSV+
171500160902     C                             %trim(pptCHR)+'VABRD2' +%trim(pptCHR)+pptCSV+
171600160902     C                             %trim(pptCHR)+'VABIND' +%trim(pptCHR)+pptCSV+
171700160902     C                             %trim(pptCHR)+'VABLOD' +%trim(pptCHR)+pptCSV+
171800160902     C                             %trim(pptCHR)+'VABCAD' +%trim(pptCHR)+pptCSV+
171900160902     C                             %trim(pptCHR)+'VABPRD' +%trim(pptCHR)+pptCSV+
172000160902     C                             %trim(pptCHR)+'VABCBO' +%trim(pptCHR)+pptCSV+
172100160902     C                             %trim(pptCHR)+'VABRMO' +%trim(pptCHR)+pptCSV+
172200160902     C                             %trim(pptCHR)+'VABRMN' +%trim(pptCHR)+pptCSV+
172300160902     C                             %trim(pptCHR)+'VABRMA' +%trim(pptCHR)+pptCSV+
172400160902     C                             %trim(pptCHR)+'VABNAS' +%trim(pptCHR)+pptCSV+
172500160902     C                             %trim(pptCHR)+'VABNCL' +%trim(pptCHR)+pptCSV+
172600160902     C                             %trim(pptCHR)+'VABPKB' +%trim(pptCHR)+pptCSV+
172700160902     C                             %trim(pptCHR)+'VABCAS' +%trim(pptCHR)+pptCSV+
172800160902     C                             %trim(pptCHR)+'VABVCA' +%trim(pptCHR)+pptCSV+
172900160902     C                             %trim(pptCHR)+'VABFFD' +%trim(pptCHR)+pptCSV+
173000160902     C                             %trim(pptCHR)+'VABIAS' +%trim(pptCHR)+pptCSV+
173100160902     C                             %trim(pptCHR)+'VABVAS' +%trim(pptCHR)+pptCSV+
173200160902     C                             %trim(pptCHR)+'VABNOT' +%trim(pptCHR)+pptCSV+
173300170510     C                             %trim(pptCHR)+'VABTSP' +%trim(pptCHR)+pptCSV+
173400160902     C                             %trim(pptCHR)+'VABNRC' +%trim(pptCHR)+pptCSV+
173500160902     C                             %trim(pptCHR)+'VABTRC' +%trim(pptCHR)+pptCSV+
173600160902     C                             %trim(pptCHR)+'VABEMD' +%trim(pptCHR)+pptCSV+
173700160902     C                             %trim(pptCHR)+'VABCEL' +%trim(pptCHR)
173800160902     C                   exsr      wriVGD
173900160902     C                   endif
174000160902     C*
174100160902     C* Quindi scrivo dati spedizione
174200160909     C   45              eval      wBufferOut =
174300170316     C                             %trim(pptCHR)+%trim(VABATB)
174400170316     C                                                    +%trim(pptCHR)+pptCSV+
174500150420     C                             %trim(pptCHR)+%trim(%editc(VABCCM:'X'))
174600150420     C                                                    +%trim(pptCHR)+pptCSV+
174700150420     C                             %trim(pptCHR)+%trim(%editc(VABLNP:'X'))
174800150420     C                                                    +%trim(pptCHR)+pptCSV+
174900150420     C                             %trim(pptCHR)+%trim(%editc(VABCTR:'X'))
175000150420     C                                                    +%trim(pptCHR)+pptCSV+
175100150420     C                             %trim(pptCHR)+%trim(VABRSD)
175200150420     C                                                    +%trim(pptCHR)+pptCSV+
175300160902     C                             %trim(pptCHR)+%trim(VABRD2)
175400160902     C                                                    +%trim(pptCHR)+pptCSV+
175500150420     C                             %trim(pptCHR)+%trim(VABIND)
175600150420     C                                                    +%trim(pptCHR)+pptCSV+
175700150420     C                             %trim(pptCHR)+%trim(VABLOD)
175800150420     C                                                    +%trim(pptCHR)+pptCSV+
175900150420     C                             %trim(pptCHR)+%trim(VABCAD)
176000150420     C                                                    +%trim(pptCHR)+pptCSV+
176100150420     C                             %trim(pptCHR)+%trim(VABPRD)
176200150420     C                                                    +%trim(pptCHR)+pptCSV+
176300150420     C                             %trim(pptCHR)+%trim(VABCBO)
176400150420     C                                                    +%trim(pptCHR)+pptCSV+
176500150420     C                             %trim(pptCHR)+%trim(VABRMO)
176600150420     C                                                    +%trim(pptCHR)+pptCSV+
176700150420     C                             %trim(pptCHR)+%trim(%editc(VABRMN:'4'))
176800150420     C                                                    +%trim(pptCHR)+pptCSV+
176900150420     C                             %trim(pptCHR)+%trim(VABRMA)
177000150420     C                                                    +%trim(pptCHR)+pptCSV+
177100150420     C                             %trim(pptCHR)+%trim(VABNAS)
177200150420     C                                                    +%trim(pptCHR)+pptCSV+
177300150420     C                             %trim(pptCHR)+%trim(%editc(VABNCL:'4'))
177400150420     C                                                    +%trim(pptCHR)+pptCSV+
177500150420     C                             %trim(pptCHR)+%trim(%editc(VABPKB:'4'))
177600150420     C                                                    +%trim(pptCHR)+pptCSV+
177700150420     C                             %trim(pptCHR)+%trim(%editc(VABCAS:'4'))
177800150420     C                                                    +%trim(pptCHR)+pptCSV+
177900150420     C                             %trim(pptCHR)+%trim(VABVCA)
178000150420     C                                                    +%trim(pptCHR)+pptCSV+
178100150420     C                             %trim(pptCHR)+%trim(VABFFD)
178200150420     C                                                    +%trim(pptCHR)+pptCSV+
178300150420     C                             %trim(pptCHR)+%trim(%editc(VABIAS:'4'))
178400150420     C                                                    +%trim(pptCHR)+pptCSV+
178500150420     C                             %trim(pptCHR)+%trim(VABVAS)
178600150420     C                                                    +%trim(pptCHR)+pptCSV+
178700150421     C                             %trim(pptCHR)+%trim(VABNOT)
178800150421     C                                                    +%trim(pptCHR)+pptCSV+
178900170510     C                             %trim(pptCHR)+%trim(VABTSP)
179000170510     C                                                    +%trim(pptCHR)+pptCSV+
179100160902     C                             %trim(pptCHR)+%trim(wVATNOT_A)
179200160902     C                                                    +%trim(pptCHR)+pptCSV+
179300160902     C                             %trim(pptCHR)+%trim(wVATNOT_B)
179400160902     C                                                    +%trim(pptCHR)+pptCSV+
179500150420     C                             %trim(pptCHR)+%trim(wVATNOT_I)
179600150420     C                                                    +%trim(pptCHR)+pptCSV+
179700150420     C                             %trim(pptCHR)+%trim(wVATNOT_S)
179800150420     C                                                    +%trim(pptCHR)
179900160909     C   45              exsr      wriVGD
180000150420     C*
180100160909     C                   endif
180200160909     C*
180300150420     C                   ENDSR
180400150420     C*------------------------------------------------------------------------*
180500150420
180600150420
180700150420
180800150420      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
180900150420     C     wriVGD        BEGSR
181000150420     C*
181100150420     C                   clear                   tivgd000
181200150420     C                   eval      vgdDTA = wBufferOut
181300150420     C                   eval      vgdTIP = 'EB'
181400150420     C                   eval      vgdKSU = vlrKSC
181500150420     C                   eval      vgdTSC = 'WW'
181600150420     C                   eval      vgdDAT = datcor
181700160906     C                   eval      vgdPRG = f_tis7prgf
181800160920     C                   eval      vgdPGM = procname
181900160906     C                   eval      vgdSTO = '?'
182000150420     C                   write     tivgd000
182100150420     C*
182200150420     C                   ENDSR
182300150420     C*------------------------------------------------------------------------*
182400160902
182500160902
182600160902     C*----------------------------------------------------*
182700160902     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
182800160902     C*----------------------------------------------------*
182900160902     C     SR_SPLIT      BEGSR
183000160902     C*
183100160902     C* Chiamo il *pgm d utilità x lo "split" della riga delle intestazioni
183200160902     C                   CALL      'XSPLIT2'
183300160902     C                   PARM                    InStringa
183400160902     C                   PARM                    InSepara
183500160902     C                   PARM      '5'           InTypeOut
183600160902     C                   PARM      ''            wSkParam
183700160902     C                   PARM                    OutErrore
183800160902     C                   PARM                    OutMsg
183900160902     C                   MOVEA     wSkParam      SkSplitCSV_5
184000160902     C*
184100160902     C                   ENDSR
184200170316
184300170316
184400170316
184500170316     C*--------------------------------------------------------
184600170316     C* CARTAB - CARICAMENTO DATI TABELLARI                   *
184700170316     C*--------------------------------------------------------
184800170316     C     CARTAB        BEGSR
184900170316     C*
185000170316     C* TABELLA '15' - NAZIONI
185100170316     C                   clear                   skNAZISO
185200170316     C                   clear                   skNAZBAR
185300170316     C                   eval      tblKUT = 1
185400170316     C                   eval      tblCOD = '15'
185500170316     C     KEYtabP       setll     tabel00f
185600170316     C     KEYtabP       reade     tabel00f
185700170316     C                   dow       not %eof(tabel00f)
185800170316     C                   if        tblFLG = *blanks
185900170316     C                   movel(p)  tblUNI        ds15
186000170316     C                   add       1             jNAZ
186100170316     C                   eval      skNAZBAR(jNAZ) = tblKEY
186200170316     C                   eval      skNAZISO(jNAZ) = §15ISO
186300170316     C                   endif
186400170316     C     KEYtabP       reade     tabel00f
186500170316     C                   enddo
186600170316     C*
186700170316     C                   ENDSR
186800170316     C***
186900170316
187000150420
187100150420
187200000613     C     *inzsr        BEGSR
187300990910     C*
187400990910     C     *entry        plist
187500990920     C                   parm                    tivlrds
187600990921     C                   parm      wrkesito      esito
187700000724     C                   parm                    prmlit
187800000710     C                   parm                    prmfir
187900010330     C*
188000010330     C* CALCOLA LA DATA CORRENTE
188100150420     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
188200150420     C                   eval      datcor = %dec(%date() : *ISO)
188300170316     C*
188400170316     C* Chiave su TABEL00F - parziale
188500170316     C     KEYtabP       KLIST
188600170316     C                   KFLD                    tblKUT
188700170316     C                   KFLD                    tblCOD
188800000613     C*
188900000613     C                   ENDSR
189000000613     C***
