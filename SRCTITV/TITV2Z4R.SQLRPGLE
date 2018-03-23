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
018200170316     C*
018300170316     C* Effettuo un primo giro per contare i record ricevuti
018400170316     C                   z-add     *zeros        wTotRec           7 0
018500170316     C                   setoff                                       70
018600170316     C                   do        *HIVAL
018700170316     C                   read      tivin00r                               70
018800170316     C  N70              add       1             wTotRec
018900170316     C  N70              enddo
019000010330     C*
019100170316     C                   z-add     *zeros        wCurRec           7 0
019200170316     C                   setoff                                       70
019300170316     C     *start        setll     tivin00r
019400030822     C                   do        *HIVAL
019500010330     C*
019600030822     C                   read      tivin00r                               70
019700170316     C  N70              add       1             wCurRec
019800160902     C*
019900160902     C* Salvo il buffer orginale ricevuto dal cliente
020000160902     C                   eval      s_vindta = vindta
020100160902     C*
020200160902     C                   if        *in70 = *off and
020300160902     C                             vindta <> *blanks and
020400160902     C                             (vinflg = *blanks
020500160902     C                              or vinflg = '0'
020600160902     C                              or vinflg = '2')
020700170316     C*
020800170316     C* Ignoro SEMPRE gli ultimi 2 record
020900170316     C                             and wCurRec < wTotRec - 1
021000160902     C*
021100160902     C* Controlli di "base" sui dati grezzi
021200160902     C                   exsr      normCSV
021300040510     C*
021400160902     C                   if        wFlgVINDTA = 1
021500010330     C*
021600010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
021700010711     C                   if        vinflg = *blanks or vinflg = '0'
021800010711     C                   clear                   vinmsg
021900010711     C                   endif
022000160902     C*
022100160902 xxx C* Porto eventualmente da minuscolo in maiuscolo le INTESTAZIONI colonne cliente
022200160908 xxx C***  minu:MAIU     XLATE     vindta        vindta
022300160902     C*
022400160902     C* Splitto i dati
022500160902     C                   CLEAR                   SkSplitCSV_5
022600160902     C                   EVAL      InStringa = vindta
022700160902     C                   EVAL      InSepara  = CharCSV
022800160902     C                   EXSR      SR_SPLIT
022900160902     C*
023000160902     C*****************************
023100160902     C* A questo punto verifico che le colonne mandatorie siano valorizzate correttamente
023200160902     C                   if        %len(%trim(SkSplitCSV_5(1))) > 0 and
023300160902     C                             %len(%trim(SkSplitCSV_5(3))) > 3 and
023400170316     C                             %len(%trim(SkSplitCSV_5(6))) > 3 and
023500170316     C                             %len(%trim(SkSplitCSV_5(8))) > 1
023600160902     C*****************************
023700010601     C*
023800160902     C* Se OK => procedo con elaborazione
023900010330     C                   exsr      inzvar
024000010605     C                   exsr      impvab
024100010601     C*
024200010604     C* Ebbene...
024300160908     C                   if        wGiro > 1
024400160908     C  N31              add       1             §CTROKVB
024500160908     C   32              add       1             §CTRMO
024600160908     C   31              add       1             §CTRNO
024700160908     C*
024800160902     C   50              exsr      wriVAB_VGD
024900160908     C  N50              exsr      wriVAB_WW
025000150420     C                   endif
025100010604     C*
025200010604     C                   if        *in31 = *off and
025300010604     C                             *in32 = *off
025400010604     C                   eval      vinflg = '1'
025500010604     C                   else
025600010604     C                   eval      vinflg = '2'
025700010604     C                   endif
025800010601     C*
025900160902     C                   else
026000160902     C                   eval      vinflg = '1'
026100010604     C                   endif
026200160902     C*
026300160902     C                   else
026400160902     C                   eval      vinflg = '1'
026500160902     C                   endif
026600010604     C*
026700010330     C                   else
026800010330     C                   eval      vinflg = '1'
026900010330     C                   endif
027000010601     C*
027100160902     C                   eval      vindta = s_vindta
027200010601     C  N70              update    tivin000
027300010330     C*
027400030822     C  N70              enddo
027500010601     C
027600010601     C*
027700160902     C* Stabilisco l'esito della traduzione
027800160902     C                   select
027900160902     C*
028000160902     C* Se non ci sono record con errori => stato OK
028100160902     C                   when      §ctrno = 0 and
028200160902     C                             §ctrmo = 0
028300010330     C                   eval      wrkesito = '0'
028400160902     C*
028500160902     C* Se nessun record OK => errore totale
028600160908     C                   when      §ctrokvb = 0
028700160902     C                   eval      wrkesito = '2'
028800160902     C*
028900160902     C* Diversamente => errore parziale/warning
029000160902     C                   other
029100160902     C                   eval      wrkesito = '1'
029200160902     C*
029300160902     C                   endsl
029400160908     C*
029500160908     C                   endif
029600160908     C
029700160908     C                   if        chkcall = '1'
029800160908     C                   eval      wrkesito = '2'
029900160908     C                   endif
030000010330     C*
030100010330     C                   if        %open(tivin00r)
030200010330     C                   close     tivin00r
030300010330     C                   endif
030400160908     C*
030500160908     C                   if        %open(fivabwwr)
030600160908     C                   close     fivabwwr
030700160908     C                   endif
030800160908     C                   if        %open(fivatwwr)
030900160908     C                   close     fivatwwr
031000160908     C                   endif
031100160908     C*
031200160908     C                   if        not *in50        and
031300160908     C                             wrkesito <> '2'  and
031400160908     C                             vlrpoi <> *zeros
031500160908     C                   exsr      invio
031600160908     C                   endif
031700010330     C*
031800010330     C                   ENDSR
031900010330     C***
032000160908
032100160908
032200160908     C*----------------------------------------------------*
032300160908     C*  SCARICAMENTO BUFFER RECORDS VAB IN "WW"
032400160908     C*----------------------------------------------------*
032500160908     C     WRIVAB_WW     BEGSR
032600160908     C*
032700160908     C* Verifica campi "particolari" VAB
032800160908     C                   exsr      repNSP
032900160909     C*
033000160909     C                   setoff                                       45
033100160909     C                   if        depSPE  = *zeros OR
033200160909     C                             depSPE <> vabNSP
033300160909     C                   seton                                        45
033400160909     C                   eval      depSPE  = vabNSP
033500160909     C                   endif
033600160909     C*
033700160909     C                   if        not *in31
033800160909     C                   exsr      EXEVATE
033900160909     C   45              exsr      EXEVATA
034000160909     C   45              exsr      EXEVATB
034100160909     C   45              exsr      EXEVATI
034200160909     C   45              exsr      EXEVATJ
034300160909     C   45              exsr      EXEVATS
034400160909     C   45              write     fivab000                                     => scarico il VAB
034500160909     C                   endif
034600160908     C*
034700160908     C                   ENDSR
034800160908
034900160908
035000160908     C*----------------------------------------------------*
035100160908     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
035200160908     C*----------------------------------------------------*
035300160908     C     REPNSP        BEGSR
035400160908     C*
035500160909     C* NSP => Se necessario stacco un numeratore da AZNUM
035600160909     C                   if        VABNSP = *zeros
035700160908     C                   clear                   TRUL33DS
035800160908     C                   eval      I33OPE = *zeros
035900160908     C                   eval      I33CNU = 302
036000160908     C                   eval      I33NUM = 1
036100160908     C                   movel     TRUL33DS      KPJBU
036200160908     C                   call      'TRUL33R'
036300160908     C                   parm                    KPJBA
036400160908     C                   movel     KPJBU         TRUL33DS
036500160908     C                   if        O33ERR = *zeros
036600160908     C                   z-add     O33NRF        VABNSP
036700160908     C                   z-add     O33NRF        VATNSP
036800160908     C                   else
036900160908     C                   SETON                                        31
037000160908     C                   EVAL      vinmsg = %trimr(vinmsg)
037100160908     C                             + ' ' + 'VABNSP VATNSP'
037200160908     C                   endif
037300160909     C                   endif
037400160908     C*
037500160908     C                   ENDSR
037600160908
037700160908
037800160908     C*----------------------------------------------------*
037900160908     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
038000160908     C*----------------------------------------------------*
038100160908     C     EXEVATA       BEGSR
038200160908     C*
038300160908     C                   EVAL      VATTRC = 'A'
038400160908     C                   EVAL      VATNOT = wVATNOT_A
038500160908     C                   exsr      wrivat                                       => scarico VAT
038600160908     C*
038700160908     C                   ENDSR
038800160908     C*----------------------------------------------------*
038900160908     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
039000160908     C*----------------------------------------------------*
039100160908     C     EXEVATB       BEGSR
039200160908     C*
039300160908     C                   EVAL      VATTRC = 'B'
039400160908     C                   EVAL      VATNOT = wVATNOT_B
039500160908     C                   exsr      wrivat                                       => scarico VAT
039600160908     C*
039700160908     C                   ENDSR
039800160908     C*----------------------------------------------------*
039900160908     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "I"
040000160908     C*----------------------------------------------------*
040100160908     C     EXEVATI       BEGSR
040200160908     C*
040300160909     C                   IF        %len(%trim(wVATNOT_I)) > 35
040400160909     C                   EVAL      VATTRC = 'I'
040500160909     C                   EVAL      VATNOT = %subst(%trim(wVATNOT_I):1:35)
040600160909     C                   exsr      wrivat                                       => scarico VAT
040700160909     C                   EVAL      VATTRC = 'J'
040800160909     C                   EVAL      VATNOT = %subst(%trim(wVATNOT_I):36)
040900160909     C                   exsr      wrivat                                       => scarico VAT
041000160909     C                   ELSE
041100160909     C                   EVAL      VATTRC = 'I'
041200160908     C                   EVAL      VATNOT = wVATNOT_I
041300160908     C                   exsr      wrivat                                       => scarico VAT
041400160909     C                   ENDIF
041500160908     C*
041600160908     C                   ENDSR
041700160908     C*----------------------------------------------------*
041800160908     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "J"
041900160908     C*----------------------------------------------------*
042000160908     C     EXEVATJ       BEGSR
042100160908     C*
042200160909     C                   IF        %len(%trim(wVATNOT_I)) <= 35
042300160908     C                   EVAL      VATTRC = 'J'
042400160908     C                   EVAL      VATNOT = wVATNOT_J
042500160908     C                   exsr      wrivat                                       => scarico VAT
042600160909     C                   ENDIF
042700160908     C*
042800160908     C                   ENDSR
042900160908     C*----------------------------------------------------*
043000160908     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "S"
043100160908     C*----------------------------------------------------*
043200160908     C     EXEVATS       BEGSR
043300160908     C*
043400160908     C                   EVAL      VATTRC = 'S'
043500160908     C                   EVAL      VATNOT = wVATNOT_S
043600160908     C                   exsr      wrivat                                       => scarico VAT
043700160908     C*
043800160908     C                   ENDSR
043900160908     C*----------------------------------------------------*
044000160908     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
044100160908     C*----------------------------------------------------*
044200160908     C     EXEVATE       BEGSR
044300160908     C*
044400160908     C                   EVAL      VATTRC = 'E'
044500160908     C                   EVAL      VATNOT = wVATNOT_E
044600160908     C                   exsr      wrivat                                       => scarico VAT
044700160908     C*
044800160908     C                   ENDSR
044900160908     C*----------------------------------------------------*
045000160908     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
045100160908     C*----------------------------------------------------*
045200160908     C     WRIVAT        BEGSR
045300160908     C*
045400160908     C* Scrivo solo se valorizzato qualcosa
045500160908     C                   IF        VATNOT <> *blanks
045600160908     C                   EVAL      VATFGS = VABFGS
045700160908     C                   EVAL      VATCCM = VABCCM
045800160908     C                   EVAL      VATLNP = VABLNP
045900160908     C                   EVAL      VATAAS = VABAAS
046000160908     C                   EVAL      VATNRS = VABNRS
046100160908     C                   EVAL      VATNSP = VABNSP
046200160908     C                   WRITE     FIVAT000
046300160908     C                   ADD       1             §CTROKVT
046400160908     C                   ENDIF
046500160908     C*
046600160908     C                   ENDSR
046700160908
046800160908
046900160908     C*----------------------------------------------------*
047000160908     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE ACCESSORIO
047100160908     C*----------------------------------------------------*
047200160908     C     PREVAT        BEGSR
047300160908     C*
047400160908     C* Compongo il nome del membro da dare al FIVATWWR
047500160908     C                   eval      parmbr = vlrhdl
047600160908     C                   movel     'M'           parmbr
047700160908     C                   eval      parccm = %subst(vlrKSC:2:7)
047800160908     C                   eval      paropz = '1'
047900160908     C*
048000160908     C* Effettuo la chiamata al CLLE preposto
048100160908     C                   call(e)   'TITVVTC'
048200160908     C                   parm                    parccm
048300160908     C                   parm                    parmbr
048400160908     C                   parm                    paropz
048500160908     C*
048600160908     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
048700160908     C                   if        %error
048800160908     C                   movel     '1'           chkcall
048900160908     C                   else
049000160908     C                   movel     '0'           chkcall
049100160908     C                   endif
049200160908     C*
049300160908     C                   ENDSR
049400160908
049500160908
049600160908      /TITLE Invio dei dati al punto operativo.
049700160908     C     invio         BEGSR
049800160908     C*
049900160908     C* 1° invio FIVAT
050000160908     C                   reset                   dscmz
050100160908     C                   move      vlrpoi        cmzdst
050200160908     C                   eval      cmzfld = 'FIVATWWR'
050300160908     C                   eval      cmzmbd = vlrhdl
050400160908     C                   eval      %subst(cmzmbd:1:1) = 'M'
050500160908     C***                if        prmfir = *blanks
050600160908     C                   eval      cmzfla = 'FIVAT00F'
050700160908     C                   eval      cmzmba = 'FIVAT00F'
050800160908     C***                else
050900160908     C***                eval      cmzfla = prmfir
051000160908     C***                eval      cmzmba = prmfir
051100160908     C***                endif
051200160908     C                   eval      cmznrr = *zeros
051300160908     C                   move      §ctrokvt      cmznrr
051400160908     C                   eval      cmzlba = vlrfl1
051500160908     C                   call(e)   'TIS711C'
051600160908     C                   parm                    dscmz
051700160908     C                   parm      *blanks       esito
051800160908     C                   if        %error
051900160908     C                             or cmzerr = '1'
052000160908     C                             or esito  = '1'
052100160908     C                   eval      wrkesito = '3'
052200160908     C                   else
052300160908     C*
052400160908     C* 2° invio FIVAB
052500160908     C                   reset                   dscmz
052600160908     C                   move      vlrpoi        cmzdst
052700160908     C                   eval      cmzfld = vlrfou
052800160908     C                   eval      cmzmbd = vlrhdl
052900160908     C                   eval      %subst(cmzmbd:1:1) = 'M'
053000160908     C***                if        prmfir = *blanks
053100160908     C                   eval      cmzfla = 'FIVAB00F'
053200160908     C                   eval      cmzmba = 'FIVAB00F'
053300160908     C***                else
053400160908     C***                eval      cmzfla = prmfir
053500160908     C***                eval      cmzmba = prmfir
053600160908     C***                endif
053700160908     C                   eval      cmznrr = *zeros
053800160908     C                   move      §ctrokvb      cmznrr
053900160908     C                   eval      cmzlba = vlrfl1
054000160908     C                   call(e)   'TIS711C'
054100160908     C                   parm                    dscmz
054200160908     C                   parm      *blanks       esito
054300160908     C                   if        %error
054400160908     C                             or cmzerr = '1'
054500160908     C                             or esito  = '1'
054600160908     C                   eval      wrkesito = '3'
054700160908     C                   endif
054800160908     C                   endif
054900160908     C*
055000160908     C                   ENDSR
055100160908     C***
055200010601
055300010601
055400010330     C*----------------------------------------------------*
055500030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
055600010330     C*----------------------------------------------------*
055700010330     C     INZVAR        BEGSR
055800010330     C*
055900030822     C* Inizializzo variabili di wrk
056000160902     C                   Z-ADD     *zeros        Num5_0            5 0
056100030822     C*
056200030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
056300160908     C                   CLEAR                   FIVAB000
056400160908     C                   CLEAR                   FIVAT000
056500170322     C                   CLEAR                   wVABRMA_1
056600170322     C                   CLEAR                   wVABRMA_2
056700160908     C                   CLEAR                   wVABNOT
056800160902     C                   CLEAR                   wVATNOT_A
056900160902     C                   CLEAR                   wVATNOT_B
057000160902     C                   CLEAR                   wVATNOT_E
057100150420     C                   CLEAR                   wVATNOT_I
057200160908     C                   CLEAR                   wVATNOT_J
057300150420     C                   CLEAR                   wVATNOT_S
057400030822     C                   CLEAR                   SkSplitCSV
057500160902     C                   CLEAR                   wFlgCAS
057600030822     C                   EVAL      i = 1
057700030822     C*
057800030822     C* Reimposto i valori di default
057900030822     C                   EXSR      DEFCAM
058000030822     C*
058100010330     C                   ENDSR
058200160902
058300160902
058400010330     C*----------------------------------------------------*
058500030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
058600010330     C*----------------------------------------------------*
058700010330     C     DEFCAM        BEGSR
058800110706     C*
058900160906     C                   SETOFF                                       7172
059000030822     C*
059100030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
059200030715     C* e delimitatore testo.
059300030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
059400170316     C*
059500170316     C* Verifico eventuali valori particolari per il carattere separatore campo
059600170316     C                   IF        CharCSV = '5'
059700170316     C                   EVAL      CharCSV = x'05'
059800170316     C                   ENDIF
059900170316     C*
060000030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
060100030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
060200150420     C                   IF        %subst(vlrppt:5:1) <> *blanks
060300150420     C                   EVAL      pptCSV = %subst(vlrppt:5:1)
060400150420     C                   ELSE
060500150420     C                   EVAL      pptCSV = ';'
060600150420     C                   ENDIF
060700150420     C                   IF        %subst(vlrppt:6:1) <> *blanks
060800150420     C                   EVAL      pptCHR = %subst(vlrppt:6:1)
060900150420     C                   ELSE
061000150420     C                   EVAL      pptCHR = ' '
061100150420     C                   ENDIF
061200090403     C*
061300090403     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
061400160902     C                   EVAL      CharSOS = CharNUM
061500030822     C*
061600030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
061700030715     C                   IF        %subst(vlrppt:1:1) = 'S'
061800160902     C                   SETON                                        50        * OUT => Download
061900030715     C                   ELSE
062000160902     C                   SETOFF                                       50        * OUT => File WW
062100030715     C                   ENDIF
062200030822     C*
062300030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
062400030822     C                   EVAL      posDaDft = 1
062500030822     C                   EVAL      posADft  = 0
062600030822     C                   EVAL      wGiroDft = 0
062700030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
062800030822     C                             posDaDft > 0
062900030822     C*
063000030822     C* Gestisco il 1° giro
063100030822     C                   IF        wGiroDft = 0
063200030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
063300030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
063400030822     C* Incremento il contatore dei "giri"
063500030822     C                   EVAL      wGiroDft = 1
063600030822     C                   ELSE
063700030822     C                   EVAL      posDaDft = posADft
063800030822     C                   ENDIF
063900030822     C* Eseguo lo scan x trovare la fine del campo corrente
064000030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
064100030822     C*
064200030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
064300030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
064400030822     C                   IF        posDaDft > 0 AND
064500030822     C                             posADft  > 0
064600110706     C* RMN
064700110706     C                   IF        %subst(
064800110706     C                             %subst(vlrppt:posDaDft+1:
064900110706     C                             posADft-posDaDft-1):1:3)
065000110706     C                             = 'RMN'
065100110706     C                   IF        %trim(%subst(
065200110706     C                             %subst(vlrppt:posDaDft+1:
065300110706     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065400110706     C                             = '*NSP'
065500160902     C                   SETON                                        71
065600110706     C                   ENDIF
065700110706     C                   ENDIF
065800060331     C* FGS
065900060331     C                   IF        %subst(
066000060331     C                             %subst(vlrppt:posDaDft+1:
066100060331     C                             posADft-posDaDft-1):1:3)
066200060331     C                             = 'FGS'
066300060331     C                   EVAL      PiStr=%trim(%subst(
066400060331     C                             %subst(vlrppt:posDaDft+1:
066500060331     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066600060331     C                   EXSR      CHKNUM
066700060331     C                   IF        PiInt=*on
066800060331     C                   Z-ADD     PiVal         VABFGS
066900060331     C                   ENDIF
067000060331     C                   ENDIF
067100030822     C* NCL
067200030822     C                   IF        %subst(
067300030822     C                             %subst(vlrppt:posDaDft+1:
067400030822     C                             posADft-posDaDft-1):1:3)
067500030822     C                             = 'NCL'
067600030822     C                   EVAL      PiStr=%trim(%subst(
067700030822     C                             %subst(vlrppt:posDaDft+1:
067800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067900030822     C                   EXSR      CHKNUM
068000030822     C                   IF        PiInt=*on
068100030822     C                   Z-ADD     PiVal         VABNCL
068200030822     C                   ENDIF
068300030822     C                   ENDIF
068400030822     C* CCM
068500030822     C                   IF        %subst(
068600030822     C                             %subst(vlrppt:posDaDft+1:
068700030822     C                             posADft-posDaDft-1):1:3)
068800030822     C                             = 'CCM'
068900030822     C                   EVAL      PiStr=%trim(%subst(
069000030822     C                             %subst(vlrppt:posDaDft+1:
069100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069200030822     C                   EXSR      CHKNUM
069300030822     C                   IF        PiInt=*on
069400030822     C                   Z-ADD     PiVal         VABCCM
069500030822     C                   ENDIF
069600030822     C                   ENDIF
069700030822     C* LNP
069800030822     C                   IF        %subst(
069900030822     C                             %subst(vlrppt:posDaDft+1:
070000030822     C                             posADft-posDaDft-1):1:3)
070100030822     C                             = 'LNP'
070200030822     C                   EVAL      PiStr=%trim(%subst(
070300030822     C                             %subst(vlrppt:posDaDft+1:
070400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070500030822     C                   EXSR      CHKNUM
070600030822     C                   IF        PiInt=*on
070700030822     C                   Z-ADD     PiVal         VABLNP
070800030822     C                   ENDIF
070900030822     C                   ENDIF
071000030822     C* NRS
071100030822     C                   IF        %subst(
071200030822     C                             %subst(vlrppt:posDaDft+1:
071300030822     C                             posADft-posDaDft-1):1:3)
071400030822     C                             = 'NRS'
071500030822     C                   EVAL      PiStr=%trim(%subst(
071600030822     C                             %subst(vlrppt:posDaDft+1:
071700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071800030822     C                   EXSR      CHKNUM
071900030822     C                   IF        PiInt=*on
072000030822     C                   Z-ADD     PiVal         VABNRS
072100030822     C                   ENDIF
072200030822     C                   ENDIF
072300160906     C* CTR
072400030822     C                   IF        %subst(
072500030822     C                             %subst(vlrppt:posDaDft+1:
072600030822     C                             posADft-posDaDft-1):1:3)
072700030822     C                             = 'CTR'
072800030822     C                   EVAL      PiStr=%trim(%subst(
072900030822     C                             %subst(vlrppt:posDaDft+1:
073000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073100030822     C                   EXSR      CHKNUM
073200030822     C                   IF        PiInt=*on
073300160906     C                   Z-ADD     PiVal         VABCTR
073400030822     C                   ENDIF
073500030822     C                   ENDIF
073600160906     C* CTE (ovvero CTR per Estero)
073700160906     C                   IF        %subst(
073800160906     C                             %subst(vlrppt:posDaDft+1:
073900160906     C                             posADft-posDaDft-1):1:3)
074000160906     C                             = 'CTE'
074100160906     C                   EVAL      PiStr=%trim(%subst(
074200160906     C                             %subst(vlrppt:posDaDft+1:
074300160906     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074400160906     C                   EXSR      CHKNUM
074500160906     C                   IF        PiInt=*on
074600160906     C                   Z-ADD     PiVal         wVABCTR_E
074700160906     C                   SETON                                        72
074800160906     C                   ENDIF
074900160906     C                   ENDIF
075000030822     C* PKB
075100030822     C                   IF        %subst(
075200030822     C                             %subst(vlrppt:posDaDft+1:
075300030822     C                             posADft-posDaDft-1):1:3)
075400030822     C                             = 'PKB'
075500030822     C                   EVAL      PiStr=%trim(%subst(
075600030822     C                             %subst(vlrppt:posDaDft+1:
075700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075800030822     C                   EXSR      CHKNUM
075900030822     C                   IF        PiNum=*on
076000030822     C                   Z-ADD     PiVal         VABPKB
076100030822     C                   ENDIF
076200030822     C                   ENDIF
076300030822     C* VLB
076400030822     C                   IF        %subst(
076500030822     C                             %subst(vlrppt:posDaDft+1:
076600030822     C                             posADft-posDaDft-1):1:3)
076700030822     C                             = 'VLB'
076800030822     C                   EVAL      PiStr=%trim(%subst(
076900030822     C                             %subst(vlrppt:posDaDft+1:
077000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077100030822     C                   EXSR      CHKNUM
077200030822     C                   IF        PiNum=*on
077300030822     C                   Z-ADD     PiVal         VABVLB
077400030822     C                   ENDIF
077500030822     C                   ENDIF
077600030822     C* QFT
077700030822     C                   IF        %subst(
077800030822     C                             %subst(vlrppt:posDaDft+1:
077900030822     C                             posADft-posDaDft-1):1:3)
078000030822     C                             = 'QFT'
078100030822     C                   EVAL      PiStr=%trim(%subst(
078200030822     C                             %subst(vlrppt:posDaDft+1:
078300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078400030822     C                   EXSR      CHKNUM
078500030822     C                   IF        PiNum=*on
078600030822     C                   Z-ADD     PiVal         VABQFT
078700030822     C                   ENDIF
078800030822     C                   ENDIF
078900170316     C* ATB
079000170316     C                   IF        %subst(
079100170316     C                             %subst(vlrppt:posDaDft+1:
079200170316     C                             posADft-posDaDft-1):1:3)
079300170316     C                             = 'ATB'
079400170316     C                   EVAL      VABATB=%trim(%subst(
079500170316     C                             %subst(vlrppt:posDaDft+1:
079600170316     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079700170316     C                   ENDIF
079800030822     C* CBO
079900030822     C                   IF        %subst(
080000030822     C                             %subst(vlrppt:posDaDft+1:
080100030822     C                             posADft-posDaDft-1):1:3)
080200030822     C                             = 'CBO'
080300030822     C                   EVAL      VABCBO=%trim(%subst(
080400030822     C                             %subst(vlrppt:posDaDft+1:
080500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080600030822     C                   ENDIF
080700030822     C* TSP
080800030822     C                   IF        %subst(
080900030822     C                             %subst(vlrppt:posDaDft+1:
081000030822     C                             posADft-posDaDft-1):1:3)
081100030822     C                             = 'TSP'
081200030822     C                   EVAL      VABTSP=%trim(%subst(
081300030822     C                             %subst(vlrppt:posDaDft+1:
081400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081500030822     C                   ENDIF
081600030822     C* VAS
081700030822     C                   IF        %subst(
081800030822     C                             %subst(vlrppt:posDaDft+1:
081900030822     C                             posADft-posDaDft-1):1:3)
082000030822     C                             = 'VAS'
082100030822     C                   EVAL      VABVAS=%trim(%subst(
082200030822     C                             %subst(vlrppt:posDaDft+1:
082300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082400030822     C                   ENDIF
082500030822     C* VCA
082600030822     C                   IF        %subst(
082700030822     C                             %subst(vlrppt:posDaDft+1:
082800030822     C                             posADft-posDaDft-1):1:3)
082900030822     C                             = 'VCA'
083000030822     C                   EVAL      VABVCA=%trim(%subst(
083100030822     C                             %subst(vlrppt:posDaDft+1:
083200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
083300030822     C                   ENDIF
083400030822     C* GCA
083500030822     C                   IF        %subst(
083600030822     C                             %subst(vlrppt:posDaDft+1:
083700030822     C                             posADft-posDaDft-1):1:3)
083800030822     C                             = 'GCA'
083900030822     C                   EVAL      VABGCA=%trim(%subst(
084000030822     C                             %subst(vlrppt:posDaDft+1:
084100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
084200030822     C                   ENDIF
084300030822     C* CTM
084400030822     C                   IF        %subst(
084500030822     C                             %subst(vlrppt:posDaDft+1:
084600030822     C                             posADft-posDaDft-1):1:3)
084700030822     C                             = 'CTM'
084800030822     C                   EVAL      VABCTM=%trim(%subst(
084900030822     C                             %subst(vlrppt:posDaDft+1:
085000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
085100030822     C                   ENDIF
085200030822     C* FFD
085300030822     C                   IF        %subst(
085400030822     C                             %subst(vlrppt:posDaDft+1:
085500030822     C                             posADft-posDaDft-1):1:3)
085600030822     C                             = 'FFD'
085700030822     C                   EVAL      VABFFD=%trim(%subst(
085800030822     C                             %subst(vlrppt:posDaDft+1:
085900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
086000030822     C                   ENDIF
086100030822     C* VAD
086200030822     C                   IF        %subst(
086300030822     C                             %subst(vlrppt:posDaDft+1:
086400030822     C                             posADft-posDaDft-1):1:3)
086500030822     C                             = 'VAD'
086600030822     C                   EVAL      VABVAD=%trim(%subst(
086700030822     C                             %subst(vlrppt:posDaDft+1:
086800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
086900030822     C                   ENDIF
087000030822     C* GMA
087100030822     C                   IF        %subst(
087200030822     C                             %subst(vlrppt:posDaDft+1:
087300030822     C                             posADft-posDaDft-1):1:3)
087400030822     C                             = 'GMA'
087500030822     C                   EVAL      VABGMA=%trim(%subst(
087600030822     C                             %subst(vlrppt:posDaDft+1:
087700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
087800030822     C                   ENDIF
087900030822     C* GGA
088000030822     C                   IF        %subst(
088100030822     C                             %subst(vlrppt:posDaDft+1:
088200030822     C                             posADft-posDaDft-1):1:3)
088300030822     C                             = 'GGA'
088400030822     C                   EVAL      VABGGA=%trim(%subst(
088500030822     C                             %subst(vlrppt:posDaDft+1:
088600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
088700030822     C                   ENDIF
088800030822     C* GVA
088900030822     C                   IF        %subst(
089000030822     C                             %subst(vlrppt:posDaDft+1:
089100030822     C                             posADft-posDaDft-1):1:3)
089200030822     C                             = 'GVA'
089300030822     C                   EVAL      VABGVA=%trim(%subst(
089400030822     C                             %subst(vlrppt:posDaDft+1:
089500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
089600030822     C                   ENDIF
089700030822     C* TC1
089800030822     C                   IF        %subst(
089900030822     C                             %subst(vlrppt:posDaDft+1:
090000030822     C                             posADft-posDaDft-1):1:3)
090100030822     C                             = 'TC1'
090200030822     C                   EVAL      VABTC1=%trim(%subst(
090300030822     C                             %subst(vlrppt:posDaDft+1:
090400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
090500030822     C                   ENDIF
090600030822     C* TC2
090700030822     C                   IF        %subst(
090800030822     C                             %subst(vlrppt:posDaDft+1:
090900030822     C                             posADft-posDaDft-1):1:3)
091000030822     C                             = 'TC2'
091100030822     C                   EVAL      VABTC2=%trim(%subst(
091200030822     C                             %subst(vlrppt:posDaDft+1:
091300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
091400030822     C                   ENDIF
091500030822     C* ...
091600030822     C                   ENDIF
091700030822     C                   ENDDO
091800020204     C*
091900010330     C                   ENDSR
092000010607     C*----------------------------------------------------*
092100021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
092200010607     C*----------------------------------------------------*
092300010607     C     IMPVAB        BEGSR
092400010607     C*
092500010607     C                   SETOFF                                       3132
092600030515     C*
092700010607     C                   MOVEL     datcor        VABAAS
092800010607     C                   MOVE      datcor        VABMGS
092900160902     C*
093000160902     C* Dopo aver splittato nei vari campi eseguo 1 prima "normalizzazione" x eliminare
093100160902     C* i caratteri di delimitazione testo e gli apici
093200160902     C                   EVAL      SkSplitCSV = SkSplitCSV_5
093300160902     C                   EVAL      i = 1
093400160902     C                   DOW       i < %elem(SkSplitCSV)
093500160902     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
093600160902     C     '''':' '      XLATE     SkSplitCSV(i) SkSplitCSV(i)
093700160902     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
093800160902     C                   EVAL      i = i + 1
093900160902     C                   ENDDO
094000010607     C*
094100030715     C**********
094200160902     C* Valorizzo la lista delle INTESTAZIONI colonne cliente
094300030715     C**********
094400030715     C                   IF        wGiro = *zeros
094500160902     C*
094600160902     C                   EVAL      wGiro = 1
094700160902     C                   EVAL      SkSplitFLD = SkSplitCSV
094800160902     C*
094900160902     C                   ELSE
095000160902     C*
095100160902     C**********
095200160902     C* Elaboro le righe di DATI cliente
095300160902     C**********
095400160902     C                   EVAL      wGiro = 2
095500030715     C*
095600030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
095700030715     C                   EVAL      i = 1
095800030715     C                   DOW       i < %elem(SkSplitFLD)
095900030715     C*
096000030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
096100030715     C                   IF        SkSplitFLD(i) <> *blanks AND
096200030715     C                             SkSplitCSV(i) <> *blanks
096300130624     C*** ATB
096400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
096500150420     C                   EVAL      VABATB = SkSplitCSV(i)
096600030721     C                   ENDIF
096700030715     C*** RSD
096800160902     C                   IF        %trim(SkSplitFLD(i)) =
096900160902     C                             'Nome completo dell acquirente'
097000030715     C                   EVAL      VABRSD = SkSplitCSV(i)
097100030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
097200030715     C     '@':'A'       XLATE     VABRSD        VABRSD
097300030715     C* ==
097400030715     C                   ENDIF
097500030721     C*** RD2
097600160902     C                   IF        %trim(SkSplitFLD(i)) =
097700160902     C                             'Indirizzo dell acquirente 2'
097800160902     C                   EVAL      VABRD2 = SkSplitCSV(i)
097900160902     C                   ENDIF
098000030715     C*** IND
098100160902     C                   IF        %trim(SkSplitFLD(i)) =
098200160902     C                             'Indirizzo dell acquirente 1'
098300030715     C                   EVAL      VABIND = SkSplitCSV(i)
098400030715     C                   ENDIF
098500030715     C*** LOD
098600160902     C                   IF        %trim(SkSplitFLD(i)) =
098700160902     C                             'Città dell acquirente'
098800030715     C                   EVAL      VABLOD = SkSplitCSV(i)
098900030715     C                   ENDIF
099000030715     C*** PRD
099100160902     C                   IF        %trim(SkSplitFLD(i)) =
099200160902     C                             'Stato dell acquirente'
099300030715     C                   EVAL      VABPRD = SkSplitCSV(i)
099400030715     C                   ENDIF
099500160902     C*** CAD
099600160902     C                   IF        %trim(SkSplitFLD(i)) =
099700160902     C                             'CAP dell acquirente'
099800160902     C                   EVAL      VABCAD = SkSplitCSV(i)
099900160902     C                   ENDIF
100000030721     C*** NZD
100100160902     C                   IF        %trim(SkSplitFLD(i)) =
100200160902     C                             'Paese dell acquirente'
100300030721     C                   EVAL      VABNZD = SkSplitCSV(i)
100400170316     C                   Z-ADD     1             jNAZ
100500170316     C     VABNZD        LOOKUP    skNAZISO(jNAZ)                         13
100600170316     C                   IF        %found
100700170316     C                   EVAL      VABNZD = skNAZBAR(jNAZ)
100800170316     C                   ENDIF
100900030721     C                   ENDIF
101000160902     C*** RMA - 1
101100160902     C                   IF        %trim(SkSplitFLD(i)) = 'ID ordine'
101200160902     C                   EVAL      wVABRMA_1 = SkSplitCSV(i)
101300150515     C                   ENDIF
101400160902     C*** RMA - 2
101500160902     C                   IF        %trim(SkSplitFLD(i)) = 'ID transazione'
101600160902     C                   EVAL      wVABRMA_2 = SkSplitCSV(i)
101700160902     C                   ENDIF
101800030715     C*** NOT
101900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
102000030715     C                   EVAL      VABNOT = SkSplitCSV(i)
102100030715     C                   ENDIF
102200030715     C*** NT2
102300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
102400030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
102500030715     C                   ENDIF
102600030715     C*** VCA
102700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
102800030715     C                   EVAL      VABVCA = SkSplitCSV(i)
102900030715     C                   ENDIF
103000030715     C*** VAS
103100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
103200030715     C                   EVAL      VABVAS = SkSplitCSV(i)
103300030715     C                   ENDIF
103400030715     C*** FFD
103500160902     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
103600030715     C                   EVAL      VABFFD = SkSplitCSV(i)
103700030715     C                   ENDIF
103800160902     C*** CBO
103900160902     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
104000160902     C                   EVAL      VABCBO = SkSplitCSV(i)
104100150420     C                   ENDIF
104200030721     C*** GC1
104300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
104400150420     C                   IF        %len(%trim(SkSplitCSV(i))) = 1
104500150420     C                   MOVEL     *blanks       wGC               1
104600150420     C                   EVAL      wGC = %trim(SkSplitCSV(i))
104700150420     C                   IF        SkSplitCSV(i) = 'M' OR
104800150420     C                             SkSplitCSV(i) = 'P'
104900150420     C                   MOVE(P)   wGC           VABGC1
105000150420     C                   ELSE
105100150420     C                   MOVEL(P)  wGC           VABGC1
105200150420     C                   ENDIF
105300150420     C                   ENDIF
105400030721     C                   ENDIF
105500030721     C*** GC2
105600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
105700120314     C                   IF        %len(%trim(SkSplitCSV(i))) = 1
105800120314     C                   MOVEL     *blanks       wGC               1
105900120314     C                   EVAL      wGC = %trim(SkSplitCSV(i))
106000120314     C                   IF        SkSplitCSV(i) = 'M' OR
106100120314     C                             SkSplitCSV(i) = 'P'
106200120314     C                   MOVE(P)   wGC           VABGC2
106300120314     C                   ELSE
106400120314     C                   MOVEL(P)  wGC           VABGC2
106500120314     C                   ENDIF
106600120314     C                   ENDIF
106700030721     C                   ENDIF
106800030721     C*** TSP
106900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
107000030721     C                   EVAL      VABTSP = SkSplitCSV(i)
107100030721     C                   ENDIF
107200030721     C*** NAS
107300160902     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
107400030721     C                   EVAL      VABNAS = SkSplitCSV(i)
107500030721     C                   ENDIF
107600030721     C*** TIC
107700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
107800030721     C                   EVAL      VABTIC = SkSplitCSV(i)
107900030721     C                   ENDIF
108000030721     C*** GCA
108100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
108200030721     C                   EVAL      VABGCA = SkSplitCSV(i)
108300030721     C                   ENDIF
108400030721     C*** XCO
108500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
108600030721     C                   EVAL      VABXCO = SkSplitCSV(i)
108700030721     C                   ENDIF
108800030721     C*** CTM
108900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
109000030721     C                   EVAL      VABCTM = SkSplitCSV(i)
109100030721     C                   ENDIF
109200030721     C*** TCR
109300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
109400030721     C                   EVAL      VABTCR = SkSplitCSV(i)
109500030721     C                   ENDIF
109600030721     C*** CTS
109700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
109800030721     C                   EVAL      VABCTS = SkSplitCSV(i)
109900030721     C                   ENDIF
110000030721     C*** FTM
110100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
110200030721     C                   EVAL      VABFTM = SkSplitCSV(i)
110300030721     C                   ENDIF
110400030721     C*** VAD
110500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
110600030721     C                   EVAL      VABVAD = SkSplitCSV(i)
110700030721     C                   ENDIF
110800030721     C*** GMA
110900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
111000030721     C                   EVAL      VABGMA = SkSplitCSV(i)
111100030721     C                   ENDIF
111200030721     C*** GGA
111300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
111400030721     C                   EVAL      VABGGA = SkSplitCSV(i)
111500030721     C                   ENDIF
111600030721     C*** GVA
111700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
111800030721     C                   EVAL      VABGVA = SkSplitCSV(i)
111900030721     C                   ENDIF
112000030721     C*** TC1
112100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
112200030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
112300030721     C                   ENDIF
112400030721     C*** TC2
112500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
112600030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
112700030721     C                   ENDIF
112800030721     C*** SCL
112900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
113000030721     C                   EVAL      VABSCL = SkSplitCSV(i)
113100030721     C                   ENDIF
113200030721     C*** RMO
113300160902     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
113400030721     C                   EVAL      VABRMO = SkSplitCSV(i)
113500030721     C                   ENDIF
113600160902     C*** CMO
113700160902     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
113800160902     C                   EVAL      VABCMO = SkSplitCSV(i)
113900160902     C                   ENDIF
114000030721     C*** NMO
114100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
114200030721     C                   EVAL      VABNMO = SkSplitCSV(i)
114300030721     C                   ENDIF
114400160902     C*** NOT_B
114500160902     C                   IF        %trim(SkSplitFLD(i)) =
114600160902     C                             'Numero di telefono dell acquirente'
114700160902     C                   EVAL      wVATNOT_B = SkSplitCSV(i)
114800160902     C                   ENDIF
114900150420     C*** NOT_I
115000160902     C                   IF        %trim(SkSplitFLD(i)) =
115100160902     C                             'Email dell acquirente'
115200150420     C                   EVAL      wVATNOT_I = SkSplitCSV(i)
115300150420     C                   ENDIF
115400150420     C*** NOT_S
115500160902     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_S'
115600150420     C                   EVAL      wVATNOT_S = SkSplitCSV(i)
115700150420     C                   ENDIF
115800030721     C*
115900070705     C* Reperisco quindi i campi numerici...
116000030721     C*** RMN
116100160902     C                   IF        %trim(SkSplitFLD(i)) = 'ID vendita'
116200030721     C                   EVAL      PiStr=SkSplitCSV(i)
116300030721     C                   EXSR      CHKNUM
116400030721     C                   IF        PiInt=*on
116500030721     C                   Z-ADD     PiVal         VABRMN
116600030721     C                   ELSE
116700030721     C                   SETON                                        32
116800030721     C                   EVAL      VABRMN = 1
116900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117000030721     C                             + ' ' + 'VABRMN'
117100030721     C                   ENDIF
117200030721     C                   ENDIF
117300030721     C*** NSP
117400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
117500030721     C                   EVAL      PiStr=SkSplitCSV(i)
117600030721     C                   EXSR      CHKNUM
117700100517     C                   IF        PiInt=*on
117800030721     C                   Z-ADD     PiVal         VABNSP
117900030721     C                   ELSE
118000030721     C                   SETON                                        32
118100030721     C                   EVAL      VABNSP = *zeros
118200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118300030721     C                             + ' ' + 'VABNSP'
118400030721     C                   ENDIF
118500030721     C                   ENDIF
118600160902     C*** NCL
118700160902     C***                IF        %trim(SkSplitFLD(i)) = 'Quantità'
118800160902     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
118900030721     C                   EVAL      PiStr=SkSplitCSV(i)
119000030721     C                   EXSR      CHKNUM
119100030721     C                   IF        PiInt=*on
119200030721     C                   Z-ADD     PiVal         VABNCL
119300030721     C                   ELSE
119400030721     C                   SETON                                        32
119500030721     C                   EVAL      VABNCL = *zeros
119600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119700030721     C                             + ' ' + 'VABNCL'
119800030721     C                   ENDIF
119900030721     C                   ENDIF
120000030721     C*** PKB
120100160902     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
120200030721     C                   EVAL      PiStr=SkSplitCSV(i)
120300030721     C                   EXSR      CHKNUM
120400030721     C                   IF        PiNum=*on
120500030721     C                   Z-ADD     PiVal         VABPKB
120600030721     C                   ELSE
120700030721     C                   SETON                                        32
120800030721     C                   EVAL      VABPKB = *zeros
120900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121000030721     C                             + ' ' + 'VABPKB'
121100030721     C                   ENDIF
121200030721     C                   ENDIF
121300160902     C*** CAS - 1
121400170317     C***                IF        %trim(SkSplitFLD(i)) =
121500170317     C***                          'Prezzo totale'
121600170317     C******                EVAL      wVABNOT = SkSplitCSV(i)
121700170317     C***  'EUR':'   '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
121800170317     C***                EVAL      PiStr=SkSplitCSV(i)
121900170317     C***                EXSR      CHKNUM
122000170317     C***                IF        PiNum=*on
122100170317     C***                Z-ADD     PiVal         VABCAS
122200170317     C***                MOVEL     'EUR'         VABVCA
122300170317     C***                ELSE
122400170317     C***                SETON                                        32
122500170317     C***                EVAL      VABCAS = *zeros
122600170317     C***                EVAL      vinmsg = %trimr(vinmsg)
122700170317     C***                          + ' ' + 'VABCAS'
122800170317     C***                ENDIF
122900170317     C***                ENDIF
123000160902     C*** CAS - 2
123100170317     C***                IF        %trim(SkSplitFLD(i)) =
123200170317     C***                          'Opzione Contrassegno'
123300170317     C***                IF        %trim(SkSplitCSV(i)) = 'Yes' OR
123400170317     C***                          %trim(SkSplitCSV(i)) = 'YES' OR
123500170317     C***                          %trim(SkSplitCSV(i)) = 'yes'
123600170317     C***                EVAL      wFlgCAS = 1
123700170317     C***                ENDIF
123800170317     C***                ENDIF
123900030721     C*** CCM
124000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
124100030721     C                   EVAL      PiStr=SkSplitCSV(i)
124200030721     C                   EXSR      CHKNUM
124300030721     C                   IF        PiInt=*on      AND
124400030721     C                             PiVal<=9999999 AND
124500030721     C                             PiVal>*zeros
124600030721     C                   Z-ADD     PiVal         VABCCM
124700030721     C                   ELSE
124800030721     C                   SETON                                        32
124900030721     C                   EVAL      VABCCM = *zeros
125000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125100030721     C                             + ' ' + 'VABCCM'
125200030721     C                   ENDIF
125300030721     C                   ENDIF
125400030721     C*** LNP
125500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
125600030721     C                   EVAL      PiStr=SkSplitCSV(i)
125700030721     C                   EXSR      CHKNUM
125800030721     C                   IF        PiInt=*on    AND
125900030721     C                             PiVal<=999   AND
126000030721     C                             PiVal>*zeros
126100030721     C                   Z-ADD     PiVal         VABLNP
126200030721     C                   ELSE
126300030721     C                   SETON                                        32
126400030721     C                   EVAL      VABLNP = *zeros
126500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126600030721     C                             + ' ' + 'VABLNP'
126700030721     C                   ENDIF
126800030721     C                   ENDIF
126900030721     C*** NRS
127000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
127100030721     C                   EVAL      PiStr=SkSplitCSV(i)
127200030721     C                   EXSR      CHKNUM
127300030721     C                   IF        PiInt=*on AND
127400030721     C                             PiVal<=99
127500030721     C                   Z-ADD     PiVal         VABNRS
127600030721     C                   ELSE
127700030721     C                   SETON                                        32
127800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127900030721     C                             + ' ' + 'VABNRS'
128000030721     C                   ENDIF
128100030721     C                   ENDIF
128200030721     C*** LNA
128300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
128400120314     C                   IF        %trim(SkSplitCSV(i)) <> *zeros  AND
128500120314     C                             %trim(SkSplitCSV(i)) <> '0'     AND
128600120314     C                             %trim(SkSplitCSV(i)) <> '000'   AND
128700120314     C                             %trim(SkSplitCSV(i)) <> *blanks
128800030721     C                   EVAL      PiStr=SkSplitCSV(i)
128900030721     C                   EXSR      CHKNUM
129000030721     C                   IF        PiInt=*on    AND
129100030721     C                             PiVal<=999   AND
129200030721     C                             PiVal>*zeros
129300030721     C                   Z-ADD     PiVal         VABLNA
129400030721     C                   ELSE
129500030721     C                   SETON                                        32
129600030721     C                   EVAL      VABLNA = *zeros
129700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129800030721     C                             + ' ' + 'VABLNA'
129900120314     C                   ENDIF
130000030721     C                   ENDIF
130100030721     C                   ENDIF
130200030721     C*** CTR
130300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
130400030721     C                   EVAL      PiStr=SkSplitCSV(i)
130500030721     C                   EXSR      CHKNUM
130600030721     C                   IF        PiInt=*on  AND
130700030721     C                             PiVal<=999
130800030721     C                   Z-ADD     PiVal         VABCTR
130900030721     C                   ELSE
131000030721     C                   SETON                                        32
131100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131200030721     C                             + ' ' + 'VABCTR'
131300030721     C                   ENDIF
131400030721     C                   ENDIF
131500030721     C*** IAS
131600160902     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
131700030721     C                   EVAL      PiStr=SkSplitCSV(i)
131800030721     C                   EXSR      CHKNUM
131900030721     C                   IF        PiNum=*on
132000030721     C                   Z-ADD     PiVal         VABIAS
132100030721     C                   ELSE
132200030721     C                   SETON                                        32
132300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132400030721     C                             + ' ' + 'VABIAS'
132500030721     C                   ENDIF
132600030721     C                   ENDIF
132700030721     C*** VLB
132800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
132900030721     C                   EVAL      PiStr=SkSplitCSV(i)
133000030721     C                   EXSR      CHKNUM
133100030721     C                   IF        PiNum=*on
133200030721     C                   Z-ADD     PiVal         VABVLB
133300030721     C                   ELSE
133400030721     C                   SETON                                        32
133500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133600030721     C                             + ' ' + 'VABVLB'
133700030721     C                   ENDIF
133800030721     C                   ENDIF
133900030721     C*** QFT
134000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
134100030721     C                   EVAL      PiStr=SkSplitCSV(i)
134200030721     C                   EXSR      CHKNUM
134300030721     C                   IF        PiNum=*on
134400030721     C                   Z-ADD     PiVal         VABQFT
134500030721     C                   ELSE
134600030721     C                   SETON                                        32
134700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134800030721     C                             + ' ' + 'VABQFT'
134900030721     C                   ENDIF
135000030721     C                   ENDIF
135100030721     C*** NCD
135200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
135300030721     C                   EVAL      PiStr=SkSplitCSV(i)
135400030721     C                   EXSR      CHKNUM
135500030721     C                   IF        PiInt=*on      AND
135600030721     C                             PiVal<=9999999
135700030721     C                   Z-ADD     PiVal         VABNCD
135800030721     C                   ELSE
135900030721     C                   SETON                                        32
136000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136100030721     C                             + ' ' + 'VABNCD'
136200030721     C                   ENDIF
136300030721     C                   ENDIF
136400030721     C*** NCA
136500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
136600030721     C                   EVAL      PiStr=SkSplitCSV(i)
136700030721     C                   EXSR      CHKNUM
136800030721     C                   IF        PiInt=*on      AND
136900030721     C                             PiVal<=9999999
137000030721     C                   Z-ADD     PiVal         VABNCA
137100030721     C                   ELSE
137200030721     C                   SETON                                        32
137300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137400030721     C                             + ' ' + 'VABNCA'
137500030721     C                   ENDIF
137600030721     C                   ENDIF
137700030721     C*** ZNC
137800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
137900030721     C                   EVAL      PiStr=SkSplitCSV(i)
138000030721     C                   EXSR      CHKNUM
138100030721     C                   IF        PiInt=*on AND
138200030721     C                             PiVal<=99
138300030721     C                   Z-ADD     PiVal         VABZNC
138400030721     C                   ELSE
138500030721     C                   SETON                                        32
138600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138700030721     C                             + ' ' + 'VABZNC'
138800030721     C                   ENDIF
138900030721     C                   ENDIF
139000030721     C*** DCR
139100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
139200030721     C                   EVAL      PiStr=SkSplitCSV(i)
139300030721     C                   EXSR      CHKNUM
139400030721     C                   IF        PiInt=*on       AND
139500030721     C                             PiVal<=99999999
139600030721     C                   Z-ADD     PiVal         VABDCR
139700030721     C                   ELSE
139800030721     C                   SETON                                        32
139900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
140000030721     C                             + ' ' + 'VABDCR'
140100030721     C                   ENDIF
140200030721     C                   ENDIF
140300030721     C*** HCR
140400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
140500030721     C                   EVAL      PiStr=SkSplitCSV(i)
140600030721     C                   EXSR      CHKNUM
140700030721     C                   IF        PiInt=*on   AND
140800030721     C                             PiVal<=9999
140900030721     C                   Z-ADD     PiVal         VABHCR
141000030721     C                   ELSE
141100030721     C                   SETON                                        32
141200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
141300030721     C                             + ' ' + 'VABHCR'
141400030721     C                   ENDIF
141500030721     C                   ENDIF
141600030721     C*** VMD
141700160902     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
141800030721     C                   EVAL      PiStr=SkSplitCSV(i)
141900030721     C                   EXSR      CHKNUM
142000030721     C                   IF        PiNum=*on
142100030721     C                   Z-ADD     PiVal         VABVMD
142200030721     C                   ELSE
142300030721     C                   SETON                                        32
142400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
142500030721     C                             + ' ' + 'VABVMD'
142600030721     C                   ENDIF
142700030721     C                   ENDIF
142800030721     C*** ANT
142900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
143000030721     C                   EVAL      PiStr=SkSplitCSV(i)
143100030721     C                   EXSR      CHKNUM
143200030721     C                   IF        PiInt=*on        AND
143300030721     C                             PiVal<=999999999
143400030721     C                   Z-ADD     PiVal         VABANT
143500030721     C                   ELSE
143600030721     C                   SETON                                        32
143700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
143800030721     C                             + ' ' + 'VABANT'
143900030721     C                   ENDIF
144000030721     C                   ENDIF
144100030715     C*
144200030715     C                   ENDIF
144300030715     C                   EVAL      i = i + 1
144400030715     C                   ENDDO
144500070705     C*
144600160902     C* Considerazioni particolari "fuori ciclo"
144700110706     C*
144800160908     C* Verifica campi "particolari" VAB
144900160908     C                   EXSR      CHKVAB
145000160908     C*
145100030715     C                   ENDIF
145200020204     C*
145300010607     C                   ENDSR
145400010607     C*----------------------------------------------------*
145500160908
145600160908
145700160908     C*----------------------------------------------------*
145800160908     C*  CONSIDERAZIONI SU CAMPI PARTICOLARI VAB
145900160908     C*----------------------------------------------------*
146000160908     C     CHKVAB        BEGSR
146100160908     C*
146200160908     C*** RMN
146300160908     C                   IF        *in71
146400160908     C                   EVAL      VABRMN = VABNSP
146500160908     C                   ENDIF
146600160908     C                   IF        VABRMN = *zeros
146700160908     C                   EVAL      VABRMN = 1
146800160908     C                   ENDIF
146900160908     C*** NZD
147000160908     C                   IF        VABNZD = 'IT'  OR
147100160908     C                             VABNZD = 'ITA' OR
147200160908     C                             VABNZD = 'Ita' OR
147300160908     C                             VABNZD = 'ita'
147400160908     C                   EVAL      VABNZD = *blanks
147500160908     C                   ENDIF
147600170317     C                   IF        VABNZD <> *blanks
147700170317     C                   EVAL      VABPRD = *blanks
147800170317     C                   ENDIF
147900160908     C*** CTR
148000160908     C                   IF        *in72
148100160908     C                   IF        VABNZD <> *blanks
148200160908     C                   EVAL      VABCTR =  wVABCTR_E
148300160908     C                   ENDIF
148400160908     C                   ENDIF
148500160908     C*** CBO
148600160908     C                   IF        VABCBO = *blanks
148700160908     C                   EVAL      VABCBO = '1'
148800160908     C                   ENDIF
148900160908     C*** NCL
149000160908     C                   IF        VABNCL = *zeros
149100160908     C                   EVAL      VABNCL = 1
149200160908     C                   ENDIF
149300160908     C*** CCM
149400160908     C                   IF        VABCCM = *zeros
149500160908     C                   EVAL      PiStr=VLRKSC
149600160908     C                   EXSR      CHKNUM
149700160908     C                   IF        PiInt=*on
149800160908     C                   Z-ADD     PiVal         VABCCM
149900160908     C                   ELSE
150000160908     C                   SETON                                        31
150100160908     C                   ENDIF
150200160908     C                   ENDIF
150300160908     C*** LNP
150400160908     C                   IF        VABLNP = *zeros
150500160908     C                   IF        VLRPOI = 046 OR
150600160908     C                             VLRPOI = 789 OR
150700160908     C                             VLRPOI = 999
150800160908     C                   MOVEL     VABCCM        VABLNP
150900160908     C                   ELSE
151000160908     C                   EVAL      VABLNP = VLRPOI
151100160908     C                   ENDIF
151200160908     C                   ENDIF
151300160908     C*** LNP
151400160908     C                   IF        VABFGS = *zeros
151500160908     C                   EVAL      VABFGS = VABLNP
151600160908     C                   ENDIF
151700160908     C*** FGS / LNP
151800160908     C                   IF        VABFGS = *zeros OR
151900160908     C                             VABLNP = *zeros
152000160908     C                   SETON                                        31
152100160908     C                   ENDIF
152200160908     C*** RMA
152300160908     C                   IF        wVABRMA_1 <> *blanks
152400160908     C                   EVAL      VABRMA = wVABRMA_1
152500160908     C                   ELSE
152600160920     C                   IF        wVABRMA_2 <> *blanks
152700160908     C                   EVAL      VABRMA = wVABRMA_2
152800160908     C                   ENDIF
152900160920     C                   ENDIF
153000160908     C*** CAS
153100160908     C                   IF        wFlgCAS = *zeros
153200160908     C                   EVAL      VABCAS = *zeros
153300160908     C                   EVAL      VABVCA = *blanks
153400160908     C                   ENDIF
153500160908     C*** NOT
153600160908     C                   IF        VABNOT = *blanks AND wVABNOT <> *blanks
153700160908     C                   EVAL      VABNOT = wVABNOT
153800160908     C                   ENDIF
153900160908     C*
154000160908     C* Se provincia nn valorizzata la reperisco
154100160908     C* tramite TISI95R a seconda dei dati d instradamento presenti
154200170316     C                   IF        VABNZD = *blanks AND
154300170316     C                             VABPRD = *blanks
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
169300170316     C                   if        wAlfa1 <  X'40' AND
169400170316     C                             wAlfa1 <> CharCSV
169500160902     C                   eval      %subst(vindta:wLenVINDTA:1) = *blanks
169600160902     C                   endif
169700160902     C*
169800160902     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
169900160902     C                   if        wAlfa1 <> CharCSV and
170000160902     C                             wAlfa1 <> CharTXT and
170100160902     C                             wAlfa1 <> CharNUM
170200160902     C                   z-add     1             wFlgVINDTA
170300160902     C                   endif
170400160902     C*
170500160902     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
170600160902     C* separatori campi
170700160902     C                   if        CharTXT <> *blanks
170800160902     C                   if        wAlfa1 = CharTXT
170900160902     C                   if        not *in80
171000160902     C                   seton                                        80
171100160902     C                   else
171200160902     C                   setoff                                       80
171300160902     C                   endif
171400160902     C                   endif
171500160902     C                   if        wAlfa1 = CharCSV and *in80 = *on
171600160902     C                   eval      %subst(vindta:wLenVINDTA:1) = CharSOS
171700160902     C                   endif
171800160902     C                   endif
171900160902     C*
172000160902     C                   enddo
172100100419     C*
172200100419     C                   ENDSR
172300100419     C***
172400150420
172500150420
172600150420      /TITLE Scrittura record in file TIVGD00F (file VAS generico download)
172700160902     C     wriVAB_VGD    BEGSR
172800160909     C*
172900160909     C* Verifica campi "particolari" VAB
173000160909     C                   exsr      repNSP
173100160909     C*
173200160909     C                   setoff                                       45
173300160909     C                   if        depSPE  = *zeros OR
173400160909     C                             depSPE <> vabNSP
173500160909     C                   seton                                        45
173600160909     C                   eval      depSPE  = vabNSP
173700160909     C                   endif
173800160909     C*
173900160909     C                   if        not *in31
174000150420     C*
174100160902     C* Se primo giro allora scrivo testata
174200160902     C                   if        wVGD_Head = *zeros
174300160902     C                   eval      wVGD_Head = 1
174400160902     C                   eval      wBufferOut =
174500170316     C                             %trim(pptCHR)+'VABATB' +%trim(pptCHR)+pptCSV+
174600160902     C                             %trim(pptCHR)+'VABCCM' +%trim(pptCHR)+pptCSV+
174700160902     C                             %trim(pptCHR)+'VABLNP' +%trim(pptCHR)+pptCSV+
174800160902     C                             %trim(pptCHR)+'VABCTR' +%trim(pptCHR)+pptCSV+
174900160902     C                             %trim(pptCHR)+'VABRSD' +%trim(pptCHR)+pptCSV+
175000160902     C                             %trim(pptCHR)+'VABRD2' +%trim(pptCHR)+pptCSV+
175100160902     C                             %trim(pptCHR)+'VABIND' +%trim(pptCHR)+pptCSV+
175200160902     C                             %trim(pptCHR)+'VABLOD' +%trim(pptCHR)+pptCSV+
175300160902     C                             %trim(pptCHR)+'VABCAD' +%trim(pptCHR)+pptCSV+
175400160902     C                             %trim(pptCHR)+'VABPRD' +%trim(pptCHR)+pptCSV+
175500170317     C                             %trim(pptCHR)+'VABNZD' +%trim(pptCHR)+pptCSV+
175600160902     C                             %trim(pptCHR)+'VABCBO' +%trim(pptCHR)+pptCSV+
175700160902     C                             %trim(pptCHR)+'VABRMO' +%trim(pptCHR)+pptCSV+
175800160902     C                             %trim(pptCHR)+'VABRMN' +%trim(pptCHR)+pptCSV+
175900160902     C                             %trim(pptCHR)+'VABRMA' +%trim(pptCHR)+pptCSV+
176000160902     C                             %trim(pptCHR)+'VABNAS' +%trim(pptCHR)+pptCSV+
176100160902     C                             %trim(pptCHR)+'VABNCL' +%trim(pptCHR)+pptCSV+
176200160902     C                             %trim(pptCHR)+'VABPKB' +%trim(pptCHR)+pptCSV+
176300160902     C                             %trim(pptCHR)+'VABCAS' +%trim(pptCHR)+pptCSV+
176400160902     C                             %trim(pptCHR)+'VABVCA' +%trim(pptCHR)+pptCSV+
176500160902     C                             %trim(pptCHR)+'VABFFD' +%trim(pptCHR)+pptCSV+
176600160902     C                             %trim(pptCHR)+'VABIAS' +%trim(pptCHR)+pptCSV+
176700160902     C                             %trim(pptCHR)+'VABVAS' +%trim(pptCHR)+pptCSV+
176800160902     C                             %trim(pptCHR)+'VABNOT' +%trim(pptCHR)+pptCSV+
176900160902     C                             %trim(pptCHR)+'VABNRC' +%trim(pptCHR)+pptCSV+
177000160902     C                             %trim(pptCHR)+'VABTRC' +%trim(pptCHR)+pptCSV+
177100160902     C                             %trim(pptCHR)+'VABEMD' +%trim(pptCHR)+pptCSV+
177200160902     C                             %trim(pptCHR)+'VABCEL' +%trim(pptCHR)
177300160902     C                   exsr      wriVGD
177400160902     C                   endif
177500160902     C*
177600160902     C* Quindi scrivo dati spedizione
177700160909     C   45              eval      wBufferOut =
177800170316     C                             %trim(pptCHR)+%trim(VABATB)
177900170316     C                                                    +%trim(pptCHR)+pptCSV+
178000150420     C                             %trim(pptCHR)+%trim(%editc(VABCCM:'X'))
178100150420     C                                                    +%trim(pptCHR)+pptCSV+
178200150420     C                             %trim(pptCHR)+%trim(%editc(VABLNP:'X'))
178300150420     C                                                    +%trim(pptCHR)+pptCSV+
178400150420     C                             %trim(pptCHR)+%trim(%editc(VABCTR:'X'))
178500150420     C                                                    +%trim(pptCHR)+pptCSV+
178600150420     C                             %trim(pptCHR)+%trim(VABRSD)
178700150420     C                                                    +%trim(pptCHR)+pptCSV+
178800160902     C                             %trim(pptCHR)+%trim(VABRD2)
178900160902     C                                                    +%trim(pptCHR)+pptCSV+
179000150420     C                             %trim(pptCHR)+%trim(VABIND)
179100150420     C                                                    +%trim(pptCHR)+pptCSV+
179200150420     C                             %trim(pptCHR)+%trim(VABLOD)
179300150420     C                                                    +%trim(pptCHR)+pptCSV+
179400150420     C                             %trim(pptCHR)+%trim(VABCAD)
179500150420     C                                                    +%trim(pptCHR)+pptCSV+
179600150420     C                             %trim(pptCHR)+%trim(VABPRD)
179700150420     C                                                    +%trim(pptCHR)+pptCSV+
179800170317     C                             %trim(pptCHR)+%trim(VABNZD)
179900170317     C                                                    +%trim(pptCHR)+pptCSV+
180000150420     C                             %trim(pptCHR)+%trim(VABCBO)
180100150420     C                                                    +%trim(pptCHR)+pptCSV+
180200150420     C                             %trim(pptCHR)+%trim(VABRMO)
180300150420     C                                                    +%trim(pptCHR)+pptCSV+
180400150420     C                             %trim(pptCHR)+%trim(%editc(VABRMN:'4'))
180500150420     C                                                    +%trim(pptCHR)+pptCSV+
180600150420     C                             %trim(pptCHR)+%trim(VABRMA)
180700150420     C                                                    +%trim(pptCHR)+pptCSV+
180800150420     C                             %trim(pptCHR)+%trim(VABNAS)
180900150420     C                                                    +%trim(pptCHR)+pptCSV+
181000150420     C                             %trim(pptCHR)+%trim(%editc(VABNCL:'4'))
181100150420     C                                                    +%trim(pptCHR)+pptCSV+
181200150420     C                             %trim(pptCHR)+%trim(%editc(VABPKB:'4'))
181300150420     C                                                    +%trim(pptCHR)+pptCSV+
181400150420     C                             %trim(pptCHR)+%trim(%editc(VABCAS:'4'))
181500150420     C                                                    +%trim(pptCHR)+pptCSV+
181600150420     C                             %trim(pptCHR)+%trim(VABVCA)
181700150420     C                                                    +%trim(pptCHR)+pptCSV+
181800150420     C                             %trim(pptCHR)+%trim(VABFFD)
181900150420     C                                                    +%trim(pptCHR)+pptCSV+
182000150420     C                             %trim(pptCHR)+%trim(%editc(VABIAS:'4'))
182100150420     C                                                    +%trim(pptCHR)+pptCSV+
182200150420     C                             %trim(pptCHR)+%trim(VABVAS)
182300150420     C                                                    +%trim(pptCHR)+pptCSV+
182400150421     C                             %trim(pptCHR)+%trim(VABNOT)
182500150421     C                                                    +%trim(pptCHR)+pptCSV+
182600160902     C                             %trim(pptCHR)+%trim(wVATNOT_A)
182700160902     C                                                    +%trim(pptCHR)+pptCSV+
182800160902     C                             %trim(pptCHR)+%trim(wVATNOT_B)
182900160902     C                                                    +%trim(pptCHR)+pptCSV+
183000150420     C                             %trim(pptCHR)+%trim(wVATNOT_I)
183100150420     C                                                    +%trim(pptCHR)+pptCSV+
183200150420     C                             %trim(pptCHR)+%trim(wVATNOT_S)
183300150420     C                                                    +%trim(pptCHR)
183400160909     C   45              exsr      wriVGD
183500160909     C*
183600160909     C                   endif
183700150420     C*
183800150420     C                   ENDSR
183900150420     C*------------------------------------------------------------------------*
184000150420
184100150420
184200150420
184300150420      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
184400150420     C     wriVGD        BEGSR
184500150420     C*
184600150420     C                   clear                   tivgd000
184700150420     C                   eval      vgdDTA = wBufferOut
184800150420     C                   eval      vgdTIP = 'EB'
184900150420     C                   eval      vgdKSU = vlrKSC
185000150420     C                   eval      vgdTSC = 'WW'
185100150420     C                   eval      vgdDAT = datcor
185200160906     C                   eval      vgdPRG = f_tis7prgf
185300160920     C                   eval      vgdPGM = procname
185400160906     C                   eval      vgdSTO = '?'
185500150420     C                   write     tivgd000
185600150420     C*
185700150420     C                   ENDSR
185800150420     C*------------------------------------------------------------------------*
185900160902
186000160902
186100160902     C*----------------------------------------------------*
186200160902     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
186300160902     C*----------------------------------------------------*
186400160902     C     SR_SPLIT      BEGSR
186500160902     C*
186600160902     C* Chiamo il *pgm d utilità x lo "split" della riga delle intestazioni
186700160902     C                   CALL      'XSPLIT2'
186800160902     C                   PARM                    InStringa
186900160902     C                   PARM                    InSepara
187000160902     C                   PARM      '5'           InTypeOut
187100160902     C                   PARM      ''            wSkParam
187200160902     C                   PARM                    OutErrore
187300160902     C                   PARM                    OutMsg
187400160902     C                   MOVEA     wSkParam      SkSplitCSV_5
187500160902     C*
187600160902     C                   ENDSR
187700170316
187800170316
187900170316
188000170316     C*--------------------------------------------------------
188100170316     C* CARTAB - CARICAMENTO DATI TABELLARI                   *
188200170316     C*--------------------------------------------------------
188300170316     C     CARTAB        BEGSR
188400170316     C*
188500170316     C* TABELLA '15' - NAZIONI
188600170316     C                   clear                   skNAZISO
188700170316     C                   clear                   skNAZBAR
188800170316     C                   eval      tblKUT = 1
188900170316     C                   eval      tblCOD = '15'
189000170316     C     KEYtabP       setll     tabel00f
189100170316     C     KEYtabP       reade     tabel00f
189200170316     C                   dow       not %eof(tabel00f)
189300170316     C                   if        tblFLG = *blanks
189400170316     C                   movel(p)  tblUNI        ds15
189500170316     C                   add       1             jNAZ
189600170316     C                   eval      skNAZBAR(jNAZ) = tblKEY
189700170316     C                   eval      skNAZISO(jNAZ) = §15ISO
189800170316     C                   endif
189900170316     C     KEYtabP       reade     tabel00f
190000170316     C                   enddo
190100170316     C*
190200170316     C                   ENDSR
190300170316     C***
190400170316
190500150420
190600150420
190700000613     C     *inzsr        BEGSR
190800990910     C*
190900990910     C     *entry        plist
191000990920     C                   parm                    tivlrds
191100990921     C                   parm      wrkesito      esito
191200000724     C                   parm                    prmlit
191300000710     C                   parm                    prmfir
191400010330     C*
191500010330     C* CALCOLA LA DATA CORRENTE
191600150420     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
191700150420     C                   eval      datcor = %dec(%date() : *ISO)
191800170316     C*
191900170316     C* Chiave su TABEL00F - parziale
192000170316     C     KEYtabP       KLIST
192100170316     C                   KFLD                    tblKUT
192200170316     C                   KFLD                    tblCOD
192300000613     C*
192400000613     C                   ENDSR
192500000613     C***
