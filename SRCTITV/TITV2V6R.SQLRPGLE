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
003200160909     D wVATNOT_J       s             70    inz
003300150420     D wVATNOT_S       s             35    inz
003400160902     D wVABRMA_1       s             15    inz
003500160902     D wVABRMA_2       s             15    inz
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
009600170316
009700170316     D*------------
009800170316     D jNAZ            s              5  0 INZ(*zeros)
009900170316     D skNAZISO        S              3    DIM(1000)
010000170316     D skNAZBAR        S              3    DIM(1000)
010100010330
010200010330
010300990921     C                   reset                   esito
010400990921     C                   reset                   wrkesito
010500010601     C*
010600170316     C                   exsr      cartab
010700160908     C                   exsr      defcam
010800160908     C   50              exsr      calprog
010900150420     C                   exsr      rwfile
011000160908     C   50              exsr      strvgd
011100160906     C                   exsr      endela
011200160906     C*
011300010601     C                   seton                                        lr
011400160906
011500160906
011600160906     C*--------------------------------------------------------
011700160906     C* STRVGD - "LIBERO" LA TRANSAZIONE (GRUPPO) CORRENTE    *
011800160906     C*--------------------------------------------------------
011900160906     C     STRVGD        BEGSR
012000160906     C*
012100160906     C
012200160906     C/EXEC SQL
012300170227     C+ UPDATE TIVGD00F SET VGDSTO = 'R'
012400170119     C+ WHERE VGDSTO = '?'     AND VGDPRG = :VGDPRG AND
012500170119     C+       VGDTIP = :VGDTIP AND VGDKSU = :VGDKSU AND
012600170119     C+       VGDTSC = :VGDTSC
012700160906     C/END-EXEC
012800160906     C
012900160906     C*
013000160906     C                   ENDSR
013100160906     C***
013200160902
013300010601
013400010601     C*--------------------------------------------------------
013500160902     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
013600010601     C*--------------------------------------------------------
013700010601     C     ENDELA        BEGSR
013800080222     C*
013900080222     C* Effettuo lancio TISI95 solo x chiusura
014000080222     C                   CLEAR                   TISI95DS
014100080222     C                   EVAL      I95TLA = 'C'
014200080222     C                   CALL      'TISI95R'
014300080222     C                   PARM                    TISI95DS
014400160908     C*
014500160908     C* Effettuo la chiamata al CLLE preposto
014600160908     C  N50              call(e)   'TITVVTC'
014700160908     C                   parm                    parccm
014800160908     C                   parm                    parmbr
014900160908     C                   parm      '2'           paropz
015000000613     C*
015100010601     C                   ENDSR
015200010601     C***
015300010601
015400010601
015500010330     C*--------------------------------------------------------
015600150420     C* RWFILE  LEGGE TIVIN00R E SCRIVE FIVABWWF              *
015700010330     C*--------------------------------------------------------
015800150420     C     RWFILE        BEGSR
015900010330     C*
016000010330     C                   if        not %open(tivin00r)
016100010330     C                   open      tivin00r
016200010330     C                   endif
016300160908     C*
016400160908     C* Eseguo operazioni di aggiunta nuovo membro file FNVAT
016500160908     C  N50              exsr      prevat
016600160908     C   50              eval      chkcall = '0'
016700160908     C*
016800160908     C                   if        chkcall = '0'
016900160908     C*
017000160908     C                   if        not %open(fivabwwr)
017100160908     C                   open      fivabwwr
017200160908     C                   endif
017300160908     C*
017400160908     C                   if        not %open(fivatwwr)
017500160908     C                   open      fivatwwr
017600160908     C                   endif
017700010330     C*
017800160908     C                   clear                   §CTROKVB          7 0
017900160908     C                   clear                   §CTROKVT          7 0
018000160908     C                   clear                   §CTRMO            7 0
018100160908     C                   clear                   §CTRNO            7 0
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
021600160902     C* A questo punto verifico che le colonne mandatorie siano valorizzate correttamente
021700160902     C                   if        %len(%trim(SkSplitCSV_5(1))) > 0 and
021800160902     C                             %len(%trim(SkSplitCSV_5(3))) > 3 and
021900160902     C                             %len(%trim(SkSplitCSV_5(6))) > 1
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
038200160908     C*----------------------------------------------------*
038300160908     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "I"
038400160908     C*----------------------------------------------------*
038500160908     C     EXEVATI       BEGSR
038600160908     C*
038700160909     C                   IF        %len(%trim(wVATNOT_I)) > 35
038800160909     C                   EVAL      VATTRC = 'I'
038900160909     C                   EVAL      VATNOT = %subst(%trim(wVATNOT_I):1:35)
039000160909     C                   exsr      wrivat                                       => scarico VAT
039100160909     C                   EVAL      VATTRC = 'J'
039200160909     C                   EVAL      VATNOT = %subst(%trim(wVATNOT_I):36)
039300160909     C                   exsr      wrivat                                       => scarico VAT
039400160909     C                   ELSE
039500160909     C                   EVAL      VATTRC = 'I'
039600160908     C                   EVAL      VATNOT = wVATNOT_I
039700160908     C                   exsr      wrivat                                       => scarico VAT
039800160909     C                   ENDIF
039900160908     C*
040000160908     C                   ENDSR
040100160908     C*----------------------------------------------------*
040200160908     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "J"
040300160908     C*----------------------------------------------------*
040400160908     C     EXEVATJ       BEGSR
040500160908     C*
040600160909     C                   IF        %len(%trim(wVATNOT_I)) <= 35
040700160908     C                   EVAL      VATTRC = 'J'
040800160908     C                   EVAL      VATNOT = wVATNOT_J
040900160908     C                   exsr      wrivat                                       => scarico VAT
041000160909     C                   ENDIF
041100160908     C*
041200160908     C                   ENDSR
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
054900170322     C                   CLEAR                   wVABRMA_1
055000170322     C                   CLEAR                   wVABRMA_2
055100160908     C                   CLEAR                   wVABNOT
055200160902     C                   CLEAR                   wVATNOT_A
055300160902     C                   CLEAR                   wVATNOT_B
055400160902     C                   CLEAR                   wVATNOT_E
055500150420     C                   CLEAR                   wVATNOT_I
055600160908     C                   CLEAR                   wVATNOT_J
055700150420     C                   CLEAR                   wVATNOT_S
055800030822     C                   CLEAR                   SkSplitCSV
055900160902     C                   CLEAR                   wFlgCAS
056000030822     C                   EVAL      i = 1
056100030822     C*
056200030822     C* Reimposto i valori di default
056300030822     C                   EXSR      DEFCAM
056400030822     C*
056500010330     C                   ENDSR
056600160902
056700160902
056800010330     C*----------------------------------------------------*
056900030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
057000010330     C*----------------------------------------------------*
057100010330     C     DEFCAM        BEGSR
057200110706     C*
057300160906     C                   SETOFF                                       7172
057400030822     C*
057500030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
057600030715     C* e delimitatore testo.
057700030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
057800170316     C*
057900170316     C* Verifico eventuali valori particolari per il carattere separatore campo
058000170316     C                   IF        CharCSV = '5'
058100170316     C                   EVAL      CharCSV = x'05'
058200170316     C                   ENDIF
058300170316     C*
058400030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
058500030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
058600150420     C                   IF        %subst(vlrppt:5:1) <> *blanks
058700150420     C                   EVAL      pptCSV = %subst(vlrppt:5:1)
058800150420     C                   ELSE
058900150420     C                   EVAL      pptCSV = ';'
059000150420     C                   ENDIF
059100150420     C                   IF        %subst(vlrppt:6:1) <> *blanks
059200150420     C                   EVAL      pptCHR = %subst(vlrppt:6:1)
059300150420     C                   ELSE
059400150420     C                   EVAL      pptCHR = ' '
059500150420     C                   ENDIF
059600090403     C*
059700090403     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
059800160902     C                   EVAL      CharSOS = CharNUM
059900030822     C*
060000030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
060100030715     C                   IF        %subst(vlrppt:1:1) = 'S'
060200160902     C                   SETON                                        50        * OUT => Download
060300030715     C                   ELSE
060400160902     C                   SETOFF                                       50        * OUT => File WW
060500030715     C                   ENDIF
060600030822     C*
060700030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
060800030822     C                   EVAL      posDaDft = 1
060900030822     C                   EVAL      posADft  = 0
061000030822     C                   EVAL      wGiroDft = 0
061100030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
061200030822     C                             posDaDft > 0
061300030822     C*
061400030822     C* Gestisco il 1° giro
061500030822     C                   IF        wGiroDft = 0
061600030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
061700030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
061800030822     C* Incremento il contatore dei "giri"
061900030822     C                   EVAL      wGiroDft = 1
062000030822     C                   ELSE
062100030822     C                   EVAL      posDaDft = posADft
062200030822     C                   ENDIF
062300030822     C* Eseguo lo scan x trovare la fine del campo corrente
062400030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
062500030822     C*
062600030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
062700030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
062800030822     C                   IF        posDaDft > 0 AND
062900030822     C                             posADft  > 0
063000110706     C* RMN
063100110706     C                   IF        %subst(
063200110706     C                             %subst(vlrppt:posDaDft+1:
063300110706     C                             posADft-posDaDft-1):1:3)
063400110706     C                             = 'RMN'
063500110706     C                   IF        %trim(%subst(
063600110706     C                             %subst(vlrppt:posDaDft+1:
063700110706     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063800110706     C                             = '*NSP'
063900160902     C                   SETON                                        71
064000110706     C                   ENDIF
064100110706     C                   ENDIF
064200060331     C* FGS
064300060331     C                   IF        %subst(
064400060331     C                             %subst(vlrppt:posDaDft+1:
064500060331     C                             posADft-posDaDft-1):1:3)
064600060331     C                             = 'FGS'
064700060331     C                   EVAL      PiStr=%trim(%subst(
064800060331     C                             %subst(vlrppt:posDaDft+1:
064900060331     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065000060331     C                   EXSR      CHKNUM
065100060331     C                   IF        PiInt=*on
065200060331     C                   Z-ADD     PiVal         VABFGS
065300060331     C                   ENDIF
065400060331     C                   ENDIF
065500030822     C* NCL
065600030822     C                   IF        %subst(
065700030822     C                             %subst(vlrppt:posDaDft+1:
065800030822     C                             posADft-posDaDft-1):1:3)
065900030822     C                             = 'NCL'
066000030822     C                   EVAL      PiStr=%trim(%subst(
066100030822     C                             %subst(vlrppt:posDaDft+1:
066200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066300030822     C                   EXSR      CHKNUM
066400030822     C                   IF        PiInt=*on
066500030822     C                   Z-ADD     PiVal         VABNCL
066600030822     C                   ENDIF
066700030822     C                   ENDIF
066800030822     C* CCM
066900030822     C                   IF        %subst(
067000030822     C                             %subst(vlrppt:posDaDft+1:
067100030822     C                             posADft-posDaDft-1):1:3)
067200030822     C                             = 'CCM'
067300030822     C                   EVAL      PiStr=%trim(%subst(
067400030822     C                             %subst(vlrppt:posDaDft+1:
067500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067600030822     C                   EXSR      CHKNUM
067700030822     C                   IF        PiInt=*on
067800030822     C                   Z-ADD     PiVal         VABCCM
067900030822     C                   ENDIF
068000030822     C                   ENDIF
068100030822     C* LNP
068200030822     C                   IF        %subst(
068300030822     C                             %subst(vlrppt:posDaDft+1:
068400030822     C                             posADft-posDaDft-1):1:3)
068500030822     C                             = 'LNP'
068600030822     C                   EVAL      PiStr=%trim(%subst(
068700030822     C                             %subst(vlrppt:posDaDft+1:
068800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068900030822     C                   EXSR      CHKNUM
069000030822     C                   IF        PiInt=*on
069100030822     C                   Z-ADD     PiVal         VABLNP
069200030822     C                   ENDIF
069300030822     C                   ENDIF
069400030822     C* NRS
069500030822     C                   IF        %subst(
069600030822     C                             %subst(vlrppt:posDaDft+1:
069700030822     C                             posADft-posDaDft-1):1:3)
069800030822     C                             = 'NRS'
069900030822     C                   EVAL      PiStr=%trim(%subst(
070000030822     C                             %subst(vlrppt:posDaDft+1:
070100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070200030822     C                   EXSR      CHKNUM
070300030822     C                   IF        PiInt=*on
070400030822     C                   Z-ADD     PiVal         VABNRS
070500030822     C                   ENDIF
070600030822     C                   ENDIF
070700160906     C* CTR
070800030822     C                   IF        %subst(
070900030822     C                             %subst(vlrppt:posDaDft+1:
071000030822     C                             posADft-posDaDft-1):1:3)
071100030822     C                             = 'CTR'
071200030822     C                   EVAL      PiStr=%trim(%subst(
071300030822     C                             %subst(vlrppt:posDaDft+1:
071400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071500030822     C                   EXSR      CHKNUM
071600030822     C                   IF        PiInt=*on
071700160906     C                   Z-ADD     PiVal         VABCTR
071800030822     C                   ENDIF
071900030822     C                   ENDIF
072000160906     C* CTE (ovvero CTR per Estero)
072100160906     C                   IF        %subst(
072200160906     C                             %subst(vlrppt:posDaDft+1:
072300160906     C                             posADft-posDaDft-1):1:3)
072400160906     C                             = 'CTE'
072500160906     C                   EVAL      PiStr=%trim(%subst(
072600160906     C                             %subst(vlrppt:posDaDft+1:
072700160906     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072800160906     C                   EXSR      CHKNUM
072900160906     C                   IF        PiInt=*on
073000160906     C                   Z-ADD     PiVal         wVABCTR_E
073100160906     C                   SETON                                        72
073200160906     C                   ENDIF
073300160906     C                   ENDIF
073400030822     C* PKB
073500030822     C                   IF        %subst(
073600030822     C                             %subst(vlrppt:posDaDft+1:
073700030822     C                             posADft-posDaDft-1):1:3)
073800030822     C                             = 'PKB'
073900030822     C                   EVAL      PiStr=%trim(%subst(
074000030822     C                             %subst(vlrppt:posDaDft+1:
074100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074200030822     C                   EXSR      CHKNUM
074300030822     C                   IF        PiNum=*on
074400030822     C                   Z-ADD     PiVal         VABPKB
074500030822     C                   ENDIF
074600030822     C                   ENDIF
074700030822     C* VLB
074800030822     C                   IF        %subst(
074900030822     C                             %subst(vlrppt:posDaDft+1:
075000030822     C                             posADft-posDaDft-1):1:3)
075100030822     C                             = 'VLB'
075200030822     C                   EVAL      PiStr=%trim(%subst(
075300030822     C                             %subst(vlrppt:posDaDft+1:
075400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075500030822     C                   EXSR      CHKNUM
075600030822     C                   IF        PiNum=*on
075700030822     C                   Z-ADD     PiVal         VABVLB
075800030822     C                   ENDIF
075900030822     C                   ENDIF
076000030822     C* QFT
076100030822     C                   IF        %subst(
076200030822     C                             %subst(vlrppt:posDaDft+1:
076300030822     C                             posADft-posDaDft-1):1:3)
076400030822     C                             = 'QFT'
076500030822     C                   EVAL      PiStr=%trim(%subst(
076600030822     C                             %subst(vlrppt:posDaDft+1:
076700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076800030822     C                   EXSR      CHKNUM
076900030822     C                   IF        PiNum=*on
077000030822     C                   Z-ADD     PiVal         VABQFT
077100030822     C                   ENDIF
077200030822     C                   ENDIF
077300170316     C* ATB
077400170316     C                   IF        %subst(
077500170316     C                             %subst(vlrppt:posDaDft+1:
077600170316     C                             posADft-posDaDft-1):1:3)
077700170316     C                             = 'ATB'
077800170316     C                   EVAL      VABATB=%trim(%subst(
077900170316     C                             %subst(vlrppt:posDaDft+1:
078000170316     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078100170316     C                   ENDIF
078200030822     C* CBO
078300030822     C                   IF        %subst(
078400030822     C                             %subst(vlrppt:posDaDft+1:
078500030822     C                             posADft-posDaDft-1):1:3)
078600030822     C                             = 'CBO'
078700030822     C                   EVAL      VABCBO=%trim(%subst(
078800030822     C                             %subst(vlrppt:posDaDft+1:
078900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079000030822     C                   ENDIF
079100030822     C* TSP
079200030822     C                   IF        %subst(
079300030822     C                             %subst(vlrppt:posDaDft+1:
079400030822     C                             posADft-posDaDft-1):1:3)
079500030822     C                             = 'TSP'
079600030822     C                   EVAL      VABTSP=%trim(%subst(
079700030822     C                             %subst(vlrppt:posDaDft+1:
079800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079900030822     C                   ENDIF
080000030822     C* VAS
080100030822     C                   IF        %subst(
080200030822     C                             %subst(vlrppt:posDaDft+1:
080300030822     C                             posADft-posDaDft-1):1:3)
080400030822     C                             = 'VAS'
080500030822     C                   EVAL      VABVAS=%trim(%subst(
080600030822     C                             %subst(vlrppt:posDaDft+1:
080700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080800030822     C                   ENDIF
080900030822     C* VCA
081000030822     C                   IF        %subst(
081100030822     C                             %subst(vlrppt:posDaDft+1:
081200030822     C                             posADft-posDaDft-1):1:3)
081300030822     C                             = 'VCA'
081400030822     C                   EVAL      VABVCA=%trim(%subst(
081500030822     C                             %subst(vlrppt:posDaDft+1:
081600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081700030822     C                   ENDIF
081800030822     C* GCA
081900030822     C                   IF        %subst(
082000030822     C                             %subst(vlrppt:posDaDft+1:
082100030822     C                             posADft-posDaDft-1):1:3)
082200030822     C                             = 'GCA'
082300030822     C                   EVAL      VABGCA=%trim(%subst(
082400030822     C                             %subst(vlrppt:posDaDft+1:
082500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082600030822     C                   ENDIF
082700030822     C* CTM
082800030822     C                   IF        %subst(
082900030822     C                             %subst(vlrppt:posDaDft+1:
083000030822     C                             posADft-posDaDft-1):1:3)
083100030822     C                             = 'CTM'
083200030822     C                   EVAL      VABCTM=%trim(%subst(
083300030822     C                             %subst(vlrppt:posDaDft+1:
083400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
083500030822     C                   ENDIF
083600030822     C* FFD
083700030822     C                   IF        %subst(
083800030822     C                             %subst(vlrppt:posDaDft+1:
083900030822     C                             posADft-posDaDft-1):1:3)
084000030822     C                             = 'FFD'
084100030822     C                   EVAL      VABFFD=%trim(%subst(
084200030822     C                             %subst(vlrppt:posDaDft+1:
084300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
084400030822     C                   ENDIF
084500030822     C* VAD
084600030822     C                   IF        %subst(
084700030822     C                             %subst(vlrppt:posDaDft+1:
084800030822     C                             posADft-posDaDft-1):1:3)
084900030822     C                             = 'VAD'
085000030822     C                   EVAL      VABVAD=%trim(%subst(
085100030822     C                             %subst(vlrppt:posDaDft+1:
085200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
085300030822     C                   ENDIF
085400030822     C* GMA
085500030822     C                   IF        %subst(
085600030822     C                             %subst(vlrppt:posDaDft+1:
085700030822     C                             posADft-posDaDft-1):1:3)
085800030822     C                             = 'GMA'
085900030822     C                   EVAL      VABGMA=%trim(%subst(
086000030822     C                             %subst(vlrppt:posDaDft+1:
086100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
086200030822     C                   ENDIF
086300030822     C* GGA
086400030822     C                   IF        %subst(
086500030822     C                             %subst(vlrppt:posDaDft+1:
086600030822     C                             posADft-posDaDft-1):1:3)
086700030822     C                             = 'GGA'
086800030822     C                   EVAL      VABGGA=%trim(%subst(
086900030822     C                             %subst(vlrppt:posDaDft+1:
087000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
087100030822     C                   ENDIF
087200030822     C* GVA
087300030822     C                   IF        %subst(
087400030822     C                             %subst(vlrppt:posDaDft+1:
087500030822     C                             posADft-posDaDft-1):1:3)
087600030822     C                             = 'GVA'
087700030822     C                   EVAL      VABGVA=%trim(%subst(
087800030822     C                             %subst(vlrppt:posDaDft+1:
087900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
088000030822     C                   ENDIF
088100030822     C* TC1
088200030822     C                   IF        %subst(
088300030822     C                             %subst(vlrppt:posDaDft+1:
088400030822     C                             posADft-posDaDft-1):1:3)
088500030822     C                             = 'TC1'
088600030822     C                   EVAL      VABTC1=%trim(%subst(
088700030822     C                             %subst(vlrppt:posDaDft+1:
088800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
088900030822     C                   ENDIF
089000030822     C* TC2
089100030822     C                   IF        %subst(
089200030822     C                             %subst(vlrppt:posDaDft+1:
089300030822     C                             posADft-posDaDft-1):1:3)
089400030822     C                             = 'TC2'
089500030822     C                   EVAL      VABTC2=%trim(%subst(
089600030822     C                             %subst(vlrppt:posDaDft+1:
089700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
089800030822     C                   ENDIF
089900030822     C* ...
090000030822     C                   ENDIF
090100030822     C                   ENDDO
090200020204     C*
090300010330     C                   ENDSR
090400010607     C*----------------------------------------------------*
090500021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
090600010607     C*----------------------------------------------------*
090700010607     C     IMPVAB        BEGSR
090800010607     C*
090900010607     C                   SETOFF                                       3132
091000030515     C*
091100010607     C                   MOVEL     datcor        VABAAS
091200010607     C                   MOVE      datcor        VABMGS
091300160902     C*
091400160902     C* Dopo aver splittato nei vari campi eseguo 1 prima "normalizzazione" x eliminare
091500160902     C* i caratteri di delimitazione testo e gli apici
091600160902     C                   EVAL      SkSplitCSV = SkSplitCSV_5
091700160902     C                   EVAL      i = 1
091800160902     C                   DOW       i < %elem(SkSplitCSV)
091900160902     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
092000160902     C     '''':' '      XLATE     SkSplitCSV(i) SkSplitCSV(i)
092100160902     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
092200160902     C                   EVAL      i = i + 1
092300160902     C                   ENDDO
092400010607     C*
092500030715     C**********
092600160902     C* Valorizzo la lista delle INTESTAZIONI colonne cliente
092700030715     C**********
092800030715     C                   IF        wGiro = *zeros
092900160902     C*
093000160902     C                   EVAL      wGiro = 1
093100160902     C                   EVAL      SkSplitFLD = SkSplitCSV
093200160902     C*
093300160902     C                   ELSE
093400160902     C*
093500160902     C**********
093600160902     C* Elaboro le righe di DATI cliente
093700160902     C**********
093800160902     C                   EVAL      wGiro = 2
093900030715     C*
094000030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
094100030715     C                   EVAL      i = 1
094200030715     C                   DOW       i < %elem(SkSplitFLD)
094300030715     C*
094400030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
094500030715     C                   IF        SkSplitFLD(i) <> *blanks AND
094600030715     C                             SkSplitCSV(i) <> *blanks
094700130624     C*** ATB
094800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
094900150420     C                   EVAL      VABATB = SkSplitCSV(i)
095000030721     C                   ENDIF
095100030715     C*** RSD
095200160902     C                   IF        %trim(SkSplitFLD(i)) =
095300160902     C                             'Nome completo dell acquirente'
095400030715     C                   EVAL      VABRSD = SkSplitCSV(i)
095500030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
095600030715     C     '@':'A'       XLATE     VABRSD        VABRSD
095700030715     C* ==
095800030715     C                   ENDIF
095900030721     C*** RD2
096000160902     C                   IF        %trim(SkSplitFLD(i)) =
096100160902     C                             'Indirizzo dell acquirente 2'
096200160902     C                   EVAL      VABRD2 = SkSplitCSV(i)
096300160902     C                   ENDIF
096400030715     C*** IND
096500160902     C                   IF        %trim(SkSplitFLD(i)) =
096600160902     C                             'Indirizzo dell acquirente 1'
096700030715     C                   EVAL      VABIND = SkSplitCSV(i)
096800030715     C                   ENDIF
096900030715     C*** LOD
097000160902     C                   IF        %trim(SkSplitFLD(i)) =
097100160902     C                             'Città dell acquirente'
097200030715     C                   EVAL      VABLOD = SkSplitCSV(i)
097300030715     C                   ENDIF
097400030715     C*** PRD
097500160902     C                   IF        %trim(SkSplitFLD(i)) =
097600160902     C                             'Stato dell acquirente'
097700030715     C                   EVAL      VABPRD = SkSplitCSV(i)
097800030715     C                   ENDIF
097900160902     C*** CAD
098000160902     C                   IF        %trim(SkSplitFLD(i)) =
098100160902     C                             'CAP dell acquirente'
098200160902     C                   EVAL      VABCAD = SkSplitCSV(i)
098300160902     C                   ENDIF
098400030721     C*** NZD
098500160902     C                   IF        %trim(SkSplitFLD(i)) =
098600160902     C                             'Paese dell acquirente'
098700030721     C                   EVAL      VABNZD = SkSplitCSV(i)
098800170316     C                   Z-ADD     1             jNAZ
098900170316     C     VABNZD        LOOKUP    skNAZISO(jNAZ)                         13
099000170316     C                   IF        %found
099100170316     C                   EVAL      VABNZD = skNAZBAR(jNAZ)
099200170316     C                   ENDIF
099300030721     C                   ENDIF
099400160902     C*** RMA - 1
099500160902     C                   IF        %trim(SkSplitFLD(i)) = 'ID ordine'
099600160902     C                   EVAL      wVABRMA_1 = SkSplitCSV(i)
099700150515     C                   ENDIF
099800160902     C*** RMA - 2
099900160902     C                   IF        %trim(SkSplitFLD(i)) = 'ID transazione'
100000160902     C                   EVAL      wVABRMA_2 = SkSplitCSV(i)
100100160902     C                   ENDIF
100200030715     C*** NOT
100300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
100400030715     C                   EVAL      VABNOT = SkSplitCSV(i)
100500030715     C                   ENDIF
100600030715     C*** NT2
100700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
100800030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
100900030715     C                   ENDIF
101000030715     C*** VCA
101100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
101200030715     C                   EVAL      VABVCA = SkSplitCSV(i)
101300030715     C                   ENDIF
101400030715     C*** VAS
101500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
101600030715     C                   EVAL      VABVAS = SkSplitCSV(i)
101700030715     C                   ENDIF
101800030715     C*** FFD
101900160902     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
102000030715     C                   EVAL      VABFFD = SkSplitCSV(i)
102100030715     C                   ENDIF
102200160902     C*** CBO
102300160902     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
102400160902     C                   EVAL      VABCBO = SkSplitCSV(i)
102500150420     C                   ENDIF
102600030721     C*** GC1
102700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
102800150420     C                   IF        %len(%trim(SkSplitCSV(i))) = 1
102900150420     C                   MOVEL     *blanks       wGC               1
103000150420     C                   EVAL      wGC = %trim(SkSplitCSV(i))
103100150420     C                   IF        SkSplitCSV(i) = 'M' OR
103200150420     C                             SkSplitCSV(i) = 'P'
103300150420     C                   MOVE(P)   wGC           VABGC1
103400150420     C                   ELSE
103500150420     C                   MOVEL(P)  wGC           VABGC1
103600150420     C                   ENDIF
103700150420     C                   ENDIF
103800030721     C                   ENDIF
103900030721     C*** GC2
104000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
104100120314     C                   IF        %len(%trim(SkSplitCSV(i))) = 1
104200120314     C                   MOVEL     *blanks       wGC               1
104300120314     C                   EVAL      wGC = %trim(SkSplitCSV(i))
104400120314     C                   IF        SkSplitCSV(i) = 'M' OR
104500120314     C                             SkSplitCSV(i) = 'P'
104600120314     C                   MOVE(P)   wGC           VABGC2
104700120314     C                   ELSE
104800120314     C                   MOVEL(P)  wGC           VABGC2
104900120314     C                   ENDIF
105000120314     C                   ENDIF
105100030721     C                   ENDIF
105200030721     C*** TSP
105300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
105400030721     C                   EVAL      VABTSP = SkSplitCSV(i)
105500030721     C                   ENDIF
105600030721     C*** NAS
105700160902     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
105800030721     C                   EVAL      VABNAS = SkSplitCSV(i)
105900030721     C                   ENDIF
106000030721     C*** TIC
106100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
106200030721     C                   EVAL      VABTIC = SkSplitCSV(i)
106300030721     C                   ENDIF
106400030721     C*** GCA
106500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
106600030721     C                   EVAL      VABGCA = SkSplitCSV(i)
106700030721     C                   ENDIF
106800030721     C*** XCO
106900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
107000030721     C                   EVAL      VABXCO = SkSplitCSV(i)
107100030721     C                   ENDIF
107200030721     C*** CTM
107300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
107400030721     C                   EVAL      VABCTM = SkSplitCSV(i)
107500030721     C                   ENDIF
107600030721     C*** TCR
107700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
107800030721     C                   EVAL      VABTCR = SkSplitCSV(i)
107900030721     C                   ENDIF
108000030721     C*** CTS
108100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
108200030721     C                   EVAL      VABCTS = SkSplitCSV(i)
108300030721     C                   ENDIF
108400030721     C*** FTM
108500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
108600030721     C                   EVAL      VABFTM = SkSplitCSV(i)
108700030721     C                   ENDIF
108800030721     C*** VAD
108900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
109000030721     C                   EVAL      VABVAD = SkSplitCSV(i)
109100030721     C                   ENDIF
109200030721     C*** GMA
109300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
109400030721     C                   EVAL      VABGMA = SkSplitCSV(i)
109500030721     C                   ENDIF
109600030721     C*** GGA
109700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
109800030721     C                   EVAL      VABGGA = SkSplitCSV(i)
109900030721     C                   ENDIF
110000030721     C*** GVA
110100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
110200030721     C                   EVAL      VABGVA = SkSplitCSV(i)
110300030721     C                   ENDIF
110400030721     C*** TC1
110500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
110600030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
110700030721     C                   ENDIF
110800030721     C*** TC2
110900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
111000030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
111100030721     C                   ENDIF
111200030721     C*** SCL
111300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
111400030721     C                   EVAL      VABSCL = SkSplitCSV(i)
111500030721     C                   ENDIF
111600030721     C*** RMO
111700160902     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
111800030721     C                   EVAL      VABRMO = SkSplitCSV(i)
111900030721     C                   ENDIF
112000160902     C*** CMO
112100160902     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
112200160902     C                   EVAL      VABCMO = SkSplitCSV(i)
112300160902     C                   ENDIF
112400030721     C*** NMO
112500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
112600030721     C                   EVAL      VABNMO = SkSplitCSV(i)
112700030721     C                   ENDIF
112800160902     C*** NOT_B
112900160902     C                   IF        %trim(SkSplitFLD(i)) =
113000160902     C                             'Numero di telefono dell acquirente'
113100160902     C                   EVAL      wVATNOT_B = SkSplitCSV(i)
113200160902     C                   ENDIF
113300150420     C*** NOT_I
113400160902     C                   IF        %trim(SkSplitFLD(i)) =
113500160902     C                             'Email dell acquirente'
113600150420     C                   EVAL      wVATNOT_I = SkSplitCSV(i)
113700150420     C                   ENDIF
113800150420     C*** NOT_S
113900160902     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_S'
114000150420     C                   EVAL      wVATNOT_S = SkSplitCSV(i)
114100150420     C                   ENDIF
114200030721     C*
114300070705     C* Reperisco quindi i campi numerici...
114400030721     C*** RMN
114500160902     C                   IF        %trim(SkSplitFLD(i)) = 'ID vendita'
114600030721     C                   EVAL      PiStr=SkSplitCSV(i)
114700030721     C                   EXSR      CHKNUM
114800030721     C                   IF        PiInt=*on
114900030721     C                   Z-ADD     PiVal         VABRMN
115000030721     C                   ELSE
115100030721     C                   SETON                                        32
115200030721     C                   EVAL      VABRMN = 1
115300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115400030721     C                             + ' ' + 'VABRMN'
115500030721     C                   ENDIF
115600030721     C                   ENDIF
115700030721     C*** NSP
115800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
115900030721     C                   EVAL      PiStr=SkSplitCSV(i)
116000030721     C                   EXSR      CHKNUM
116100100517     C                   IF        PiInt=*on
116200030721     C                   Z-ADD     PiVal         VABNSP
116300030721     C                   ELSE
116400030721     C                   SETON                                        32
116500030721     C                   EVAL      VABNSP = *zeros
116600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116700030721     C                             + ' ' + 'VABNSP'
116800030721     C                   ENDIF
116900030721     C                   ENDIF
117000160902     C*** NCL
117100160902     C***                IF        %trim(SkSplitFLD(i)) = 'Quantità'
117200160902     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
117300030721     C                   EVAL      PiStr=SkSplitCSV(i)
117400030721     C                   EXSR      CHKNUM
117500030721     C                   IF        PiInt=*on
117600030721     C                   Z-ADD     PiVal         VABNCL
117700030721     C                   ELSE
117800030721     C                   SETON                                        32
117900030721     C                   EVAL      VABNCL = *zeros
118000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118100030721     C                             + ' ' + 'VABNCL'
118200030721     C                   ENDIF
118300030721     C                   ENDIF
118400030721     C*** PKB
118500160902     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
118600030721     C                   EVAL      PiStr=SkSplitCSV(i)
118700030721     C                   EXSR      CHKNUM
118800030721     C                   IF        PiNum=*on
118900030721     C                   Z-ADD     PiVal         VABPKB
119000030721     C                   ELSE
119100030721     C                   SETON                                        32
119200030721     C                   EVAL      VABPKB = *zeros
119300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119400030721     C                             + ' ' + 'VABPKB'
119500030721     C                   ENDIF
119600030721     C                   ENDIF
119700160902     C*** CAS - 1
119800160902     C                   IF        %trim(SkSplitFLD(i)) =
119900160902     C                             'Prezzo totale'
120000160906     C***                EVAL      wVABNOT = SkSplitCSV(i)
120100160902     C     'EUR':'   '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
120200160902     C                   EVAL      PiStr=SkSplitCSV(i)
120300160902     C                   EXSR      CHKNUM
120400160902     C                   IF        PiNum=*on
120500160902     C                   Z-ADD     PiVal         VABCAS
120600160902     C                   MOVEL     'EUR'         VABVCA
120700160902     C                   ELSE
120800160902     C                   SETON                                        32
120900160902     C                   EVAL      VABCAS = *zeros
121000160902     C                   EVAL      vinmsg = %trimr(vinmsg)
121100160902     C                             + ' ' + 'VABCAS'
121200160902     C                   ENDIF
121300160902     C                   ENDIF
121400160902     C*** CAS - 2
121500160902     C                   IF        %trim(SkSplitFLD(i)) =
121600160902     C                             'Opzione Contrassegno'
121700160906     C                   IF        %trim(SkSplitCSV(i)) = 'Yes' OR
121800160906     C                             %trim(SkSplitCSV(i)) = 'YES' OR
121900160906     C                             %trim(SkSplitCSV(i)) = 'yes'
122000160902     C                   EVAL      wFlgCAS = 1
122100160902     C                   ENDIF
122200030721     C                   ENDIF
122300030721     C*** CCM
122400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
122500030721     C                   EVAL      PiStr=SkSplitCSV(i)
122600030721     C                   EXSR      CHKNUM
122700030721     C                   IF        PiInt=*on      AND
122800030721     C                             PiVal<=9999999 AND
122900030721     C                             PiVal>*zeros
123000030721     C                   Z-ADD     PiVal         VABCCM
123100030721     C                   ELSE
123200030721     C                   SETON                                        32
123300030721     C                   EVAL      VABCCM = *zeros
123400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123500030721     C                             + ' ' + 'VABCCM'
123600030721     C                   ENDIF
123700030721     C                   ENDIF
123800030721     C*** LNP
123900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
124000030721     C                   EVAL      PiStr=SkSplitCSV(i)
124100030721     C                   EXSR      CHKNUM
124200030721     C                   IF        PiInt=*on    AND
124300030721     C                             PiVal<=999   AND
124400030721     C                             PiVal>*zeros
124500030721     C                   Z-ADD     PiVal         VABLNP
124600030721     C                   ELSE
124700030721     C                   SETON                                        32
124800030721     C                   EVAL      VABLNP = *zeros
124900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125000030721     C                             + ' ' + 'VABLNP'
125100030721     C                   ENDIF
125200030721     C                   ENDIF
125300030721     C*** NRS
125400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
125500030721     C                   EVAL      PiStr=SkSplitCSV(i)
125600030721     C                   EXSR      CHKNUM
125700030721     C                   IF        PiInt=*on AND
125800030721     C                             PiVal<=99
125900030721     C                   Z-ADD     PiVal         VABNRS
126000030721     C                   ELSE
126100030721     C                   SETON                                        32
126200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126300030721     C                             + ' ' + 'VABNRS'
126400030721     C                   ENDIF
126500030721     C                   ENDIF
126600030721     C*** LNA
126700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
126800120314     C                   IF        %trim(SkSplitCSV(i)) <> *zeros  AND
126900120314     C                             %trim(SkSplitCSV(i)) <> '0'     AND
127000120314     C                             %trim(SkSplitCSV(i)) <> '000'   AND
127100120314     C                             %trim(SkSplitCSV(i)) <> *blanks
127200030721     C                   EVAL      PiStr=SkSplitCSV(i)
127300030721     C                   EXSR      CHKNUM
127400030721     C                   IF        PiInt=*on    AND
127500030721     C                             PiVal<=999   AND
127600030721     C                             PiVal>*zeros
127700030721     C                   Z-ADD     PiVal         VABLNA
127800030721     C                   ELSE
127900030721     C                   SETON                                        32
128000030721     C                   EVAL      VABLNA = *zeros
128100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128200030721     C                             + ' ' + 'VABLNA'
128300120314     C                   ENDIF
128400030721     C                   ENDIF
128500030721     C                   ENDIF
128600030721     C*** CTR
128700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
128800030721     C                   EVAL      PiStr=SkSplitCSV(i)
128900030721     C                   EXSR      CHKNUM
129000030721     C                   IF        PiInt=*on  AND
129100030721     C                             PiVal<=999
129200030721     C                   Z-ADD     PiVal         VABCTR
129300030721     C                   ELSE
129400030721     C                   SETON                                        32
129500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129600030721     C                             + ' ' + 'VABCTR'
129700030721     C                   ENDIF
129800030721     C                   ENDIF
129900030721     C*** IAS
130000160902     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
130100030721     C                   EVAL      PiStr=SkSplitCSV(i)
130200030721     C                   EXSR      CHKNUM
130300030721     C                   IF        PiNum=*on
130400030721     C                   Z-ADD     PiVal         VABIAS
130500030721     C                   ELSE
130600030721     C                   SETON                                        32
130700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130800030721     C                             + ' ' + 'VABIAS'
130900030721     C                   ENDIF
131000030721     C                   ENDIF
131100030721     C*** VLB
131200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
131300030721     C                   EVAL      PiStr=SkSplitCSV(i)
131400030721     C                   EXSR      CHKNUM
131500030721     C                   IF        PiNum=*on
131600030721     C                   Z-ADD     PiVal         VABVLB
131700030721     C                   ELSE
131800030721     C                   SETON                                        32
131900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132000030721     C                             + ' ' + 'VABVLB'
132100030721     C                   ENDIF
132200030721     C                   ENDIF
132300030721     C*** QFT
132400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
132500030721     C                   EVAL      PiStr=SkSplitCSV(i)
132600030721     C                   EXSR      CHKNUM
132700030721     C                   IF        PiNum=*on
132800030721     C                   Z-ADD     PiVal         VABQFT
132900030721     C                   ELSE
133000030721     C                   SETON                                        32
133100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133200030721     C                             + ' ' + 'VABQFT'
133300030721     C                   ENDIF
133400030721     C                   ENDIF
133500030721     C*** NCD
133600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
133700030721     C                   EVAL      PiStr=SkSplitCSV(i)
133800030721     C                   EXSR      CHKNUM
133900030721     C                   IF        PiInt=*on      AND
134000030721     C                             PiVal<=9999999
134100030721     C                   Z-ADD     PiVal         VABNCD
134200030721     C                   ELSE
134300030721     C                   SETON                                        32
134400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134500030721     C                             + ' ' + 'VABNCD'
134600030721     C                   ENDIF
134700030721     C                   ENDIF
134800030721     C*** NCA
134900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
135000030721     C                   EVAL      PiStr=SkSplitCSV(i)
135100030721     C                   EXSR      CHKNUM
135200030721     C                   IF        PiInt=*on      AND
135300030721     C                             PiVal<=9999999
135400030721     C                   Z-ADD     PiVal         VABNCA
135500030721     C                   ELSE
135600030721     C                   SETON                                        32
135700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135800030721     C                             + ' ' + 'VABNCA'
135900030721     C                   ENDIF
136000030721     C                   ENDIF
136100030721     C*** ZNC
136200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
136300030721     C                   EVAL      PiStr=SkSplitCSV(i)
136400030721     C                   EXSR      CHKNUM
136500030721     C                   IF        PiInt=*on AND
136600030721     C                             PiVal<=99
136700030721     C                   Z-ADD     PiVal         VABZNC
136800030721     C                   ELSE
136900030721     C                   SETON                                        32
137000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137100030721     C                             + ' ' + 'VABZNC'
137200030721     C                   ENDIF
137300030721     C                   ENDIF
137400030721     C*** DCR
137500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
137600030721     C                   EVAL      PiStr=SkSplitCSV(i)
137700030721     C                   EXSR      CHKNUM
137800030721     C                   IF        PiInt=*on       AND
137900030721     C                             PiVal<=99999999
138000030721     C                   Z-ADD     PiVal         VABDCR
138100030721     C                   ELSE
138200030721     C                   SETON                                        32
138300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138400030721     C                             + ' ' + 'VABDCR'
138500030721     C                   ENDIF
138600030721     C                   ENDIF
138700030721     C*** HCR
138800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
138900030721     C                   EVAL      PiStr=SkSplitCSV(i)
139000030721     C                   EXSR      CHKNUM
139100030721     C                   IF        PiInt=*on   AND
139200030721     C                             PiVal<=9999
139300030721     C                   Z-ADD     PiVal         VABHCR
139400030721     C                   ELSE
139500030721     C                   SETON                                        32
139600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
139700030721     C                             + ' ' + 'VABHCR'
139800030721     C                   ENDIF
139900030721     C                   ENDIF
140000030721     C*** VMD
140100160902     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
140200030721     C                   EVAL      PiStr=SkSplitCSV(i)
140300030721     C                   EXSR      CHKNUM
140400030721     C                   IF        PiNum=*on
140500030721     C                   Z-ADD     PiVal         VABVMD
140600030721     C                   ELSE
140700030721     C                   SETON                                        32
140800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
140900030721     C                             + ' ' + 'VABVMD'
141000030721     C                   ENDIF
141100030721     C                   ENDIF
141200030721     C*** ANT
141300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
141400030721     C                   EVAL      PiStr=SkSplitCSV(i)
141500030721     C                   EXSR      CHKNUM
141600030721     C                   IF        PiInt=*on        AND
141700030721     C                             PiVal<=999999999
141800030721     C                   Z-ADD     PiVal         VABANT
141900030721     C                   ELSE
142000030721     C                   SETON                                        32
142100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
142200030721     C                             + ' ' + 'VABANT'
142300030721     C                   ENDIF
142400030721     C                   ENDIF
142500030715     C*
142600030715     C                   ENDIF
142700030715     C                   EVAL      i = i + 1
142800030715     C                   ENDDO
142900070705     C*
143000160902     C* Considerazioni particolari "fuori ciclo"
143100110706     C*
143200160908     C* Verifica campi "particolari" VAB
143300160908     C                   EXSR      CHKVAB
143400160908     C*
143500030715     C                   ENDIF
143600020204     C*
143700010607     C                   ENDSR
143800010607     C*----------------------------------------------------*
143900160908
144000160908
144100160908     C*----------------------------------------------------*
144200160908     C*  CONSIDERAZIONI SU CAMPI PARTICOLARI VAB
144300160908     C*----------------------------------------------------*
144400160908     C     CHKVAB        BEGSR
144500160908     C*
144600160908     C*** RMN
144700160908     C                   IF        *in71
144800160908     C                   EVAL      VABRMN = VABNSP
144900160908     C                   ENDIF
145000160908     C                   IF        VABRMN = *zeros
145100160908     C                   EVAL      VABRMN = 1
145200160908     C                   ENDIF
145300160908     C*** NZD
145400160908     C                   IF        VABNZD = 'IT'  OR
145500160908     C                             VABNZD = 'ITA' OR
145600160908     C                             VABNZD = 'Ita' OR
145700160908     C                             VABNZD = 'ita'
145800160908     C                   EVAL      VABNZD = *blanks
145900160908     C                   ENDIF
146000170317     C                   IF        VABNZD <> *blanks
146100170317     C                   EVAL      VABPRD = *blanks
146200170317     C                   ENDIF
146300160908     C*** CTR
146400160908     C                   IF        *in72
146500160908     C                   IF        VABNZD <> *blanks
146600160908     C                   EVAL      VABCTR =  wVABCTR_E
146700160908     C                   ENDIF
146800160908     C                   ENDIF
146900160908     C*** CBO
147000160908     C                   IF        VABCBO = *blanks
147100160908     C                   EVAL      VABCBO = '1'
147200160908     C                   ENDIF
147300160908     C*** NCL
147400160908     C                   IF        VABNCL = *zeros
147500160908     C                   EVAL      VABNCL = 1
147600160908     C                   ENDIF
147700160908     C*** CCM
147800160908     C                   IF        VABCCM = *zeros
147900160908     C                   EVAL      PiStr=VLRKSC
148000160908     C                   EXSR      CHKNUM
148100160908     C                   IF        PiInt=*on
148200160908     C                   Z-ADD     PiVal         VABCCM
148300160908     C                   ELSE
148400160908     C                   SETON                                        31
148500160908     C                   ENDIF
148600160908     C                   ENDIF
148700160908     C*** LNP
148800160908     C                   IF        VABLNP = *zeros
148900160908     C                   IF        VLRPOI = 046 OR
149000160908     C                             VLRPOI = 789 OR
149100160908     C                             VLRPOI = 999
149200160908     C                   MOVEL     VABCCM        VABLNP
149300160908     C                   ELSE
149400160908     C                   EVAL      VABLNP = VLRPOI
149500160908     C                   ENDIF
149600160908     C                   ENDIF
149700160908     C*** LNP
149800160908     C                   IF        VABFGS = *zeros
149900160908     C                   EVAL      VABFGS = VABLNP
150000160908     C                   ENDIF
150100160908     C*** FGS / LNP
150200160908     C                   IF        VABFGS = *zeros OR
150300160908     C                             VABLNP = *zeros
150400160908     C                   SETON                                        31
150500160908     C                   ENDIF
150600160908     C*** RMA
150700160908     C                   IF        wVABRMA_1 <> *blanks
150800160908     C                   EVAL      VABRMA = wVABRMA_1
150900160908     C                   ELSE
151000160920     C                   IF        wVABRMA_2 <> *blanks
151100160908     C                   EVAL      VABRMA = wVABRMA_2
151200160908     C                   ENDIF
151300160920     C                   ENDIF
151400160908     C*** CAS
151500160908     C                   IF        wFlgCAS = *zeros
151600160908     C                   EVAL      VABCAS = *zeros
151700160908     C                   EVAL      VABVCA = *blanks
151800160908     C                   ENDIF
151900160908     C*** NOT
152000160908     C                   IF        VABNOT = *blanks AND wVABNOT <> *blanks
152100160908     C                   EVAL      VABNOT = wVABNOT
152200160908     C                   ENDIF
152300160908     C*
152400160908     C* Se provincia nn valorizzata la reperisco
152500160908     C* tramite TISI95R a seconda dei dati d instradamento presenti
152600170316     C                   IF        VABNZD = *blanks AND
152700170316     C                             VABPRD = *blanks
152800160908     C                   CLEAR                   TISI95DS
152900160908     C                   EVAL      I95TCN = '3'
153000160908     C                   Z-ADD     datcor        I95DAT
153100160908     C                   EVAL      I95NAR = VABNZD
153200160908     C                   EVAL      I95CAP = VABCAD
153300160908     C                   EVAL      I95LOC = VABLOD
153400160908     C                   CALL      'TISI95R'
153500160908     C                   PARM                    TISI95DS
153600160908     C                   EVAL      VABPRD = O95PRV
153700160908     C                   ENDIF
153800160908     C*
153900160908     C* Eseguo routine finale x considerazioni specifiche su importi/divise
154000160908     C                   EXSR      CHKIMPDIV
154100160908     C*
154200160908     C                   ENDSR
154300160908     C***
154400010601
154500020204
154600020204     C*----------------------------------------------------*
154700020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
154800020204     C*----------------------------------------------------*
154900020204     C     CHKIMPDIV     BEGSR
155000060524     C*
155100060524     C* Verifico il codice bolla in base al tipo porto e alla presenza o meno del contrassegno
155200160906     C                   IF        vabCAS  > *zeros OR
155300160906     C                             wFlgCAS > *zeros
155400060524     C                   IF        vabCBO = '1'
155500060524     C                   EVAL      vabCBO = '4'
155600060524     C                   ENDIF
155700060524     C                   IF        vabCBO = '2'
155800060524     C                   EVAL      vabCBO = '6'
155900060524     C                   ENDIF
156000060524     C                   ENDIF
156100020204     C*
156200020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
156300020204     C                   Z-ADD     *zeros        wrkDec            9 9
156400020204     C*
156500020204     C* Come prima cosa effettuo considerazioni sulla divisa
156600020204     C                   IF        vabIAS > *zeros
156700150420     C                   IF        vabVAS =  *blanks
156800150420     C                   EVAL      vabVAS =  'EUR'
156900020204     C                   ENDIF
157000020204     C                   ENDIF
157100020204     C*
157200020204     C                   IF        vabCAS > *zeros
157300150420     C                   IF        vabVCA <> *blanks
157400150420     C                   EVAL      vabVCA =  'EUR'
157500020204     C                   ENDIF
157600020204     C                   ENDIF
157700020204     C*
157800020204     C                   IF        vabVMD > *zeros
157900150420     C                   IF        vabVAD <> *blanks
158000150420     C                   EVAL      vabVAD =  'EUR'
158100020204     C                   ENDIF
158200020204     C                   ENDIF
158300020204     C*
158400020204     C                   ENDSR
158500020204     C***
158600010330
158700010330
158800010330     C*----------------------------------------------------*
158900010330     C*  CONTROLLO NUMERICITA' CAMPI
159000010330     C*----------------------------------------------------*
159100010330     C     CHKNUM        BEGSR
159200010330     C*
159300010606     C                   IF        PiDecChr = *blanks
159400030715     C                   EVAL      PiDecChr = CharNUM
159500010606     C                   ENDIF
159600010606     C*
159700010606     C                   CALL(e)   'ISNUMERIC'
159800010330     C                   PARM                    PiStr            30
159900010606     C                   PARM                    PiDecChr          1
160000010330     C                   PARM      *ZEROS        PiVal            30 9
160100010330     C                   PARM      '0'           PiInt             1
160200010330     C                   PARM      '0'           PiNum             1
160300010330     C                   IF        %error
160400010606     C                   EVAL      PiNum=*off
160500010330     C                   ENDIF
160600010330     C*
160700010330     C                   ENDSR
160800010330     C***
160900160906
161000160906
161100160906      /TITLE Valorizzazione Progressivo Applicazione
161200160906     C     calprog       begsr
161300160906     C*
161400160906     C                   movel     *blanks       dwlisv            2
161500160906     C                   movel     *all'0'       dwlprg           10
161600160906     C                   z-add     *zeros        wrkprg            8 0
161700160906     C*
161800160906     C                   eval      dwlisv = vlrisv
161900160906     C*
162000170324     C     *start        setll     tis7prgf
162100160906     C                   read(e)   tis7prgf
162200160906     C                   if        not %error
162300160906     C                   eval      dwlprg = f_tis7prgf
162400160906     C*
162500160906     C                   move(p)   dwlprg        wrkprg
162600160906     C                   add       1             wrkprg
162700160906     C                   move(p)   wrkprg        dwlprg
162800160906     C                   movel     dwlisv        dwlprg
162900160906     C*
163000160906     C                   eval      f_tis7prgf = dwlprg
163100160906     C                   update    tis7prg0
163200170324     C                   else
163300170324     C                   eval      f_tis7prgf = 'XX00000000'
163400160906     C                   endif
163500160906     C*
163600160906     C                   endsr
163700050421
163800160908
163900160908     C     *pssr         BEGSR
164000160908     C*
164100160908     C                   if        %open(tivin00r)
164200160908     C                   close     tivin00r
164300160908     C                   endif
164400160908     C                   if        %open(fivabwwr)
164500160908     C                   close     fivabwwr
164600160908     C                   endif
164700160908     C                   if        %open(fivatwwr)
164800160908     C                   close     fivatwwr
164900160908     C                   endif
165000160908     C*
165100160908     C* Effettuo la chiamata al CLLE preposto
165200160908     C  N50              call(e)   'TITVVTC'
165300160908     C                   parm                    parccm
165400160908     C                   parm                    parmbr
165500160908     C                   parm      '2'           paropz
165600160908     C*
165700160908     C                   eval      wrkesito = '2'
165800160908     C*
165900160908     C                   seton                                        LR
166000160908     C*
166100160908     C                   ENDSR     '*CANCL'
166200160908     C***
166300100419
166400100419
166500160902     C     normCSV       BEGSR
166600160902     C*
166700160902     C                   z-add     *zeros        wLenVINDTA        4 0
166800160902     C                   z-add     *zeros        wFlgVINDTA        1 0
166900160902     C                   setoff                                       80
167000160902     C*
167100160902     C                   dow       wLenVINDTA < %len(vindta)
167200160902     C                   eval      wLenVINDTA = wLenVINDTA + 1
167300160902     C                   movel     *blanks       wAlfa1            1
167400160902     C                   eval      wAlfa1 = %subst(vindta:wLenVINDTA:1)
167500160902     C*
167600160902     C* Se byte inferiore a HEX40 è un byte "non stampabile" per cui considero *blank
167700170316     C                   if        wAlfa1 <  X'40' AND
167800170316     C                             wAlfa1 <> CharCSV
167900160902     C                   eval      %subst(vindta:wLenVINDTA:1) = *blanks
168000160902     C                   endif
168100160902     C*
168200160902     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
168300160902     C                   if        wAlfa1 <> CharCSV and
168400160902     C                             wAlfa1 <> CharTXT and
168500160902     C                             wAlfa1 <> CharNUM
168600160902     C                   z-add     1             wFlgVINDTA
168700160902     C                   endif
168800160902     C*
168900160902     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
169000160902     C* separatori campi
169100160902     C                   if        CharTXT <> *blanks
169200160902     C                   if        wAlfa1 = CharTXT
169300160902     C                   if        not *in80
169400160902     C                   seton                                        80
169500160902     C                   else
169600160902     C                   setoff                                       80
169700160902     C                   endif
169800160902     C                   endif
169900160902     C                   if        wAlfa1 = CharCSV and *in80 = *on
170000160902     C                   eval      %subst(vindta:wLenVINDTA:1) = CharSOS
170100160902     C                   endif
170200160902     C                   endif
170300160902     C*
170400160902     C                   enddo
170500100419     C*
170600100419     C                   ENDSR
170700100419     C***
170800150420
170900150420
171000150420      /TITLE Scrittura record in file TIVGD00F (file VAS generico download)
171100160902     C     wriVAB_VGD    BEGSR
171200160909     C*
171300160909     C* Verifica campi "particolari" VAB
171400160909     C                   exsr      repNSP
171500160909     C*
171600160909     C                   setoff                                       45
171700160909     C                   if        depSPE  = *zeros OR
171800160909     C                             depSPE <> vabNSP
171900160909     C                   seton                                        45
172000160909     C                   eval      depSPE  = vabNSP
172100160909     C                   endif
172200160909     C*
172300160909     C                   if        not *in31
172400150420     C*
172500160902     C* Se primo giro allora scrivo testata
172600160902     C                   if        wVGD_Head = *zeros
172700160902     C                   eval      wVGD_Head = 1
172800160902     C                   eval      wBufferOut =
172900170316     C                             %trim(pptCHR)+'VABATB' +%trim(pptCHR)+pptCSV+
173000160902     C                             %trim(pptCHR)+'VABCCM' +%trim(pptCHR)+pptCSV+
173100160902     C                             %trim(pptCHR)+'VABLNP' +%trim(pptCHR)+pptCSV+
173200160902     C                             %trim(pptCHR)+'VABCTR' +%trim(pptCHR)+pptCSV+
173300160902     C                             %trim(pptCHR)+'VABRSD' +%trim(pptCHR)+pptCSV+
173400160902     C                             %trim(pptCHR)+'VABRD2' +%trim(pptCHR)+pptCSV+
173500160902     C                             %trim(pptCHR)+'VABIND' +%trim(pptCHR)+pptCSV+
173600160902     C                             %trim(pptCHR)+'VABLOD' +%trim(pptCHR)+pptCSV+
173700160902     C                             %trim(pptCHR)+'VABCAD' +%trim(pptCHR)+pptCSV+
173800160902     C                             %trim(pptCHR)+'VABPRD' +%trim(pptCHR)+pptCSV+
173900160902     C                             %trim(pptCHR)+'VABCBO' +%trim(pptCHR)+pptCSV+
174000160902     C                             %trim(pptCHR)+'VABRMO' +%trim(pptCHR)+pptCSV+
174100160902     C                             %trim(pptCHR)+'VABRMN' +%trim(pptCHR)+pptCSV+
174200160902     C                             %trim(pptCHR)+'VABRMA' +%trim(pptCHR)+pptCSV+
174300160902     C                             %trim(pptCHR)+'VABNAS' +%trim(pptCHR)+pptCSV+
174400160902     C                             %trim(pptCHR)+'VABNCL' +%trim(pptCHR)+pptCSV+
174500160902     C                             %trim(pptCHR)+'VABPKB' +%trim(pptCHR)+pptCSV+
174600160902     C                             %trim(pptCHR)+'VABCAS' +%trim(pptCHR)+pptCSV+
174700160902     C                             %trim(pptCHR)+'VABVCA' +%trim(pptCHR)+pptCSV+
174800160902     C                             %trim(pptCHR)+'VABFFD' +%trim(pptCHR)+pptCSV+
174900160902     C                             %trim(pptCHR)+'VABIAS' +%trim(pptCHR)+pptCSV+
175000160902     C                             %trim(pptCHR)+'VABVAS' +%trim(pptCHR)+pptCSV+
175100160902     C                             %trim(pptCHR)+'VABNOT' +%trim(pptCHR)+pptCSV+
175200160902     C                             %trim(pptCHR)+'VABNRC' +%trim(pptCHR)+pptCSV+
175300160902     C                             %trim(pptCHR)+'VABTRC' +%trim(pptCHR)+pptCSV+
175400160902     C                             %trim(pptCHR)+'VABEMD' +%trim(pptCHR)+pptCSV+
175500160902     C                             %trim(pptCHR)+'VABCEL' +%trim(pptCHR)
175600160902     C                   exsr      wriVGD
175700160902     C                   endif
175800160902     C*
175900160902     C* Quindi scrivo dati spedizione
176000160909     C   45              eval      wBufferOut =
176100170316     C                             %trim(pptCHR)+%trim(VABATB)
176200170316     C                                                    +%trim(pptCHR)+pptCSV+
176300150420     C                             %trim(pptCHR)+%trim(%editc(VABCCM:'X'))
176400150420     C                                                    +%trim(pptCHR)+pptCSV+
176500150420     C                             %trim(pptCHR)+%trim(%editc(VABLNP:'X'))
176600150420     C                                                    +%trim(pptCHR)+pptCSV+
176700150420     C                             %trim(pptCHR)+%trim(%editc(VABCTR:'X'))
176800150420     C                                                    +%trim(pptCHR)+pptCSV+
176900150420     C                             %trim(pptCHR)+%trim(VABRSD)
177000150420     C                                                    +%trim(pptCHR)+pptCSV+
177100160902     C                             %trim(pptCHR)+%trim(VABRD2)
177200160902     C                                                    +%trim(pptCHR)+pptCSV+
177300150420     C                             %trim(pptCHR)+%trim(VABIND)
177400150420     C                                                    +%trim(pptCHR)+pptCSV+
177500150420     C                             %trim(pptCHR)+%trim(VABLOD)
177600150420     C                                                    +%trim(pptCHR)+pptCSV+
177700150420     C                             %trim(pptCHR)+%trim(VABCAD)
177800150420     C                                                    +%trim(pptCHR)+pptCSV+
177900150420     C                             %trim(pptCHR)+%trim(VABPRD)
178000150420     C                                                    +%trim(pptCHR)+pptCSV+
178100150420     C                             %trim(pptCHR)+%trim(VABCBO)
178200150420     C                                                    +%trim(pptCHR)+pptCSV+
178300150420     C                             %trim(pptCHR)+%trim(VABRMO)
178400150420     C                                                    +%trim(pptCHR)+pptCSV+
178500150420     C                             %trim(pptCHR)+%trim(%editc(VABRMN:'4'))
178600150420     C                                                    +%trim(pptCHR)+pptCSV+
178700150420     C                             %trim(pptCHR)+%trim(VABRMA)
178800150420     C                                                    +%trim(pptCHR)+pptCSV+
178900150420     C                             %trim(pptCHR)+%trim(VABNAS)
179000150420     C                                                    +%trim(pptCHR)+pptCSV+
179100150420     C                             %trim(pptCHR)+%trim(%editc(VABNCL:'4'))
179200150420     C                                                    +%trim(pptCHR)+pptCSV+
179300150420     C                             %trim(pptCHR)+%trim(%editc(VABPKB:'4'))
179400150420     C                                                    +%trim(pptCHR)+pptCSV+
179500150420     C                             %trim(pptCHR)+%trim(%editc(VABCAS:'4'))
179600150420     C                                                    +%trim(pptCHR)+pptCSV+
179700150420     C                             %trim(pptCHR)+%trim(VABVCA)
179800150420     C                                                    +%trim(pptCHR)+pptCSV+
179900150420     C                             %trim(pptCHR)+%trim(VABFFD)
180000150420     C                                                    +%trim(pptCHR)+pptCSV+
180100150420     C                             %trim(pptCHR)+%trim(%editc(VABIAS:'4'))
180200150420     C                                                    +%trim(pptCHR)+pptCSV+
180300150420     C                             %trim(pptCHR)+%trim(VABVAS)
180400150420     C                                                    +%trim(pptCHR)+pptCSV+
180500150421     C                             %trim(pptCHR)+%trim(VABNOT)
180600150421     C                                                    +%trim(pptCHR)+pptCSV+
180700160902     C                             %trim(pptCHR)+%trim(wVATNOT_A)
180800160902     C                                                    +%trim(pptCHR)+pptCSV+
180900160902     C                             %trim(pptCHR)+%trim(wVATNOT_B)
181000160902     C                                                    +%trim(pptCHR)+pptCSV+
181100150420     C                             %trim(pptCHR)+%trim(wVATNOT_I)
181200150420     C                                                    +%trim(pptCHR)+pptCSV+
181300150420     C                             %trim(pptCHR)+%trim(wVATNOT_S)
181400150420     C                                                    +%trim(pptCHR)
181500160909     C   45              exsr      wriVGD
181600160909     C*
181700160909     C                   endif
181800150420     C*
181900150420     C                   ENDSR
182000150420     C*------------------------------------------------------------------------*
182100150420
182200150420
182300150420
182400150420      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
182500150420     C     wriVGD        BEGSR
182600150420     C*
182700150420     C                   clear                   tivgd000
182800150420     C                   eval      vgdDTA = wBufferOut
182900150420     C                   eval      vgdTIP = 'EB'
183000150420     C                   eval      vgdKSU = vlrKSC
183100150420     C                   eval      vgdTSC = 'WW'
183200150420     C                   eval      vgdDAT = datcor
183300160906     C                   eval      vgdPRG = f_tis7prgf
183400160920     C                   eval      vgdPGM = procname
183500160906     C                   eval      vgdSTO = '?'
183600150420     C                   write     tivgd000
183700150420     C*
183800150420     C                   ENDSR
183900150420     C*------------------------------------------------------------------------*
184000160902
184100160902
184200160902     C*----------------------------------------------------*
184300160902     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
184400160902     C*----------------------------------------------------*
184500160902     C     SR_SPLIT      BEGSR
184600160902     C*
184700160902     C* Chiamo il *pgm d utilità x lo "split" della riga delle intestazioni
184800160902     C                   CALL      'XSPLIT2'
184900160902     C                   PARM                    InStringa
185000160902     C                   PARM                    InSepara
185100160902     C                   PARM      '5'           InTypeOut
185200160902     C                   PARM      ''            wSkParam
185300160902     C                   PARM                    OutErrore
185400160902     C                   PARM                    OutMsg
185500160902     C                   MOVEA     wSkParam      SkSplitCSV_5
185600160902     C*
185700160902     C                   ENDSR
185800170316
185900170316
186000170316
186100170316     C*--------------------------------------------------------
186200170316     C* CARTAB - CARICAMENTO DATI TABELLARI                   *
186300170316     C*--------------------------------------------------------
186400170316     C     CARTAB        BEGSR
186500170316     C*
186600170316     C* TABELLA '15' - NAZIONI
186700170316     C                   clear                   skNAZISO
186800170316     C                   clear                   skNAZBAR
186900170316     C                   eval      tblKUT = 1
187000170316     C                   eval      tblCOD = '15'
187100170316     C     KEYtabP       setll     tabel00f
187200170316     C     KEYtabP       reade     tabel00f
187300170316     C                   dow       not %eof(tabel00f)
187400170316     C                   if        tblFLG = *blanks
187500170316     C                   movel(p)  tblUNI        ds15
187600170316     C                   add       1             jNAZ
187700170316     C                   eval      skNAZBAR(jNAZ) = tblKEY
187800170316     C                   eval      skNAZISO(jNAZ) = §15ISO
187900170316     C                   endif
188000170316     C     KEYtabP       reade     tabel00f
188100170316     C                   enddo
188200170316     C*
188300170316     C                   ENDSR
188400170316     C***
188500170316
188600150420
188700150420
188800000613     C     *inzsr        BEGSR
188900990910     C*
189000990910     C     *entry        plist
189100990920     C                   parm                    tivlrds
189200990921     C                   parm      wrkesito      esito
189300000724     C                   parm                    prmlit
189400000710     C                   parm                    prmfir
189500010330     C*
189600010330     C* CALCOLA LA DATA CORRENTE
189700150420     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
189800150420     C                   eval      datcor = %dec(%date() : *ISO)
189900170316     C*
190000170316     C* Chiave su TABEL00F - parziale
190100170316     C     KEYtabP       KLIST
190200170316     C                   KFLD                    tblKUT
190300170316     C                   KFLD                    tblCOD
190400000613     C*
190500000613     C                   ENDSR
190600000613     C***
