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
004700170316
004800170316     D*------------
004900170316     D jNAZ            s              5  0 INZ(*zeros)
005000170316     D skNAZISO        S              3    DIM(1000)
005100170316     D skNAZBAR        S              3    DIM(1000)
005200160902
005300160908
005400160908     D*------------------
005500160908     D* DS REPERIMENTO NUMERATORE
005600160908     D*------------------
005700160908     D trul33ds      e ds                  inz
005800160908     D*------------------
005900160908     D* DS ARCHITETTURA
006000160908     D*------------------
006100160908     D KPJBA         e ds                  inz
006200160908
006300160902
006400160902     D*------------------
006500160902     D* VARIABILI X LA CHIAMATA AL *PGM UTILITA' "XSPLIT"
006600160902     D*------------------
006700160902     D InStringa       S          65535A   VARYING                              (stringa input)
006800160902     D InSepara        S             10A                                        (separatore)
006900160902     D InTypeOut       S              1A                                        (tipo output)
007000160902     D wSkParam        S          65535A                                        (output)
007100160902     D OutErrore       S              1A                                        (flag errore)
007200160902     D OutMsg          S             80A                                        (messaggio errore)
007300160902     D SkSplitCSV_5    S            256    DIM(256)
007400160902
007500160902
007600030715     D*------------------
007700030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
007800030715     D*------------------
007900160902     D SkSplitFLD      S                   LIKE(SkSplitCSV_5) DIM(256)
008000160902     D SkSplitCSV      S                   LIKE(SkSplitCSV_5) DIM(256)
008100160902     D CharCSV         S              1
008200160902     D CharTXT         S              1
008300160902     D CharNUM         S              1
008400160902     D CharSOS         S              1
008500160902     D i               s              3  0 INZ(1)
008600030715     D wGiro           s              1  0 INZ(*zeros)
008700030822     D*------------------
008800030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
008900030822     D*------------------
009000160902     D posDaDft        S              3  0 INZ(*zeros)
009100160902     D posADft         S              3  0 INZ(*zeros)
009200160902     D wGiroDft        s              1  0 INZ(*zeros)
009300160908
009400160908
009500031201     D*------------------
009600031201     D* Costanti
009700031201     D*------------------
009800160902     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
009900160908     D MAIU            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
010000150420
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
012200170119     C/EXEC SQL
012300170227     C+ UPDATE TIVGD00F SET VGDSTO = 'R'
012400170119     C+ WHERE VGDSTO = '?'     AND VGDPRG = :VGDPRG AND
012500170119     C+       VGDTIP = :VGDTIP AND VGDKSU = :VGDKSU AND
012600170119     C+       VGDTSC = :VGDTSC
012700170119     C/END-EXEC
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
021600160909 xxx C* A questo punto verifico che le colonne mandatorie siano valorizzate correttamente
021700170315     C                   if        %len(%trim(SkSplitCSV_5(2)))  > 0 and
021800170315     C                             %len(%trim(SkSplitCSV_5(17))) > 3 and
021900170315     C                             %len(%trim(SkSplitCSV_5(18))) > 3 and
022000170315     C                             %len(%trim(SkSplitCSV_5(21))) > 2 and
022100170315     C                             %len(%trim(SkSplitCSV_5(23))) > 2
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
030600160908
030700160908
030800160908     C*----------------------------------------------------*
030900160908     C*  SCARICAMENTO BUFFER RECORDS VAB IN "WW"
031000160908     C*----------------------------------------------------*
031100160908     C     WRIVAB_WW     BEGSR
031200160908     C*
031300160908     C* Verifica campi "particolari" VAB
031400160908     C                   exsr      repNSP
031500160909     C*
031600160909     C                   setoff                                       45
031700160909     C                   if        depSPE  = *zeros OR
031800160909     C                             depSPE <> vabNSP
031900160909     C                   seton                                        45
032000160909     C                   eval      depSPE  = vabNSP
032100160909     C                   endif
032200160909     C*
032300160909     C                   if        not *in31
032400160909     C                   exsr      EXEVATE
032500160909     C   45              exsr      EXEVATA
032600160909     C   45              exsr      EXEVATB
032700160909     C   45              exsr      EXEVATI
032800160909     C   45              exsr      EXEVATJ
032900160909     C   45              exsr      EXEVATS
033000160909     C   45              write     fivab000                                     => scarico il VAB
033100160909     C                   endif
033200160908     C*
033300160908     C                   ENDSR
033400160908
033500160908
033600160908     C*----------------------------------------------------*
033700160908     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
033800160908     C*----------------------------------------------------*
033900160908     C     REPNSP        BEGSR
034000160908     C*
034100160909     C* NSP => Se necessario stacco un numeratore da AZNUM
034200160909     C                   if        VABNSP = *zeros
034300160908     C                   clear                   TRUL33DS
034400160908     C                   eval      I33OPE = *zeros
034500160908     C                   eval      I33CNU = 302
034600160908     C                   eval      I33NUM = 1
034700160908     C                   movel     TRUL33DS      KPJBU
034800160908     C                   call      'TRUL33R'
034900160908     C                   parm                    KPJBA
035000160908     C                   movel     KPJBU         TRUL33DS
035100160908     C                   if        O33ERR = *zeros
035200160908     C                   z-add     O33NRF        VABNSP
035300160908     C                   z-add     O33NRF        VATNSP
035400160908     C                   else
035500160908     C                   SETON                                        31
035600160908     C                   EVAL      vinmsg = %trimr(vinmsg)
035700160908     C                             + ' ' + 'VABNSP VATNSP'
035800160908     C                   endif
035900160909     C                   endif
036000160908     C*
036100160908     C                   ENDSR
036200160908
036300160908
036400160908     C*----------------------------------------------------*
036500160908     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
036600160908     C*----------------------------------------------------*
036700160908     C     EXEVATA       BEGSR
036800160908     C*
036900160908     C                   EVAL      VATTRC = 'A'
037000160908     C                   EVAL      VATNOT = wVATNOT_A
037100160908     C                   exsr      wrivat                                       => scarico VAT
037200160908     C*
037300160908     C                   ENDSR
037400160908     C*----------------------------------------------------*
037500160908     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
037600160908     C*----------------------------------------------------*
037700160908     C     EXEVATB       BEGSR
037800160908     C*
037900160908     C                   EVAL      VATTRC = 'B'
038000160908     C                   EVAL      VATNOT = wVATNOT_B
038100160908     C                   exsr      wrivat                                       => scarico VAT
038200160908     C*
038300160908     C                   ENDSR
038400160909     C*----------------------------------------------------*
038500160909     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "I"
038600160909     C*----------------------------------------------------*
038700160909     C     EXEVATI       BEGSR
038800160909     C*
038900160909     C                   IF        %len(%trim(wVATNOT_I)) > 35
039000160909     C                   EVAL      VATTRC = 'I'
039100160909     C                   EVAL      VATNOT = %subst(%trim(wVATNOT_I):1:35)
039200160909     C                   exsr      wrivat                                       => scarico VAT
039300160909     C                   EVAL      VATTRC = 'J'
039400160909     C                   EVAL      VATNOT = %subst(%trim(wVATNOT_I):36)
039500160909     C                   exsr      wrivat                                       => scarico VAT
039600160909     C                   ELSE
039700160909     C                   EVAL      VATTRC = 'I'
039800160909     C                   EVAL      VATNOT = wVATNOT_I
039900160909     C                   exsr      wrivat                                       => scarico VAT
040000160909     C                   ENDIF
040100160909     C*
040200160909     C                   ENDSR
040300160909     C*----------------------------------------------------*
040400160909     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "J"
040500160909     C*----------------------------------------------------*
040600160909     C     EXEVATJ       BEGSR
040700160909     C*
040800160909     C                   IF        %len(%trim(wVATNOT_I)) <= 35
040900160909     C                   EVAL      VATTRC = 'J'
041000160909     C                   EVAL      VATNOT = wVATNOT_J
041100160909     C                   exsr      wrivat                                       => scarico VAT
041200160909     C                   ENDIF
041300160909     C*
041400160909     C                   ENDSR
041500160908     C*----------------------------------------------------*
041600160908     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "S"
041700160908     C*----------------------------------------------------*
041800160908     C     EXEVATS       BEGSR
041900160908     C*
042000160908     C                   EVAL      VATTRC = 'S'
042100160908     C                   EVAL      VATNOT = wVATNOT_S
042200160908     C                   exsr      wrivat                                       => scarico VAT
042300160908     C*
042400160908     C                   ENDSR
042500160908     C*----------------------------------------------------*
042600160908     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
042700160908     C*----------------------------------------------------*
042800160908     C     EXEVATE       BEGSR
042900160908     C*
043000160908     C                   EVAL      VATTRC = 'E'
043100160908     C                   EVAL      VATNOT = wVATNOT_E
043200160908     C                   exsr      wrivat                                       => scarico VAT
043300160908     C*
043400160908     C                   ENDSR
043500160908     C*----------------------------------------------------*
043600160908     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
043700160908     C*----------------------------------------------------*
043800160908     C     WRIVAT        BEGSR
043900160908     C*
044000160908     C* Scrivo solo se valorizzato qualcosa
044100160908     C                   IF        VATNOT <> *blanks
044200160908     C                   EVAL      VATFGS = VABFGS
044300160908     C                   EVAL      VATCCM = VABCCM
044400160908     C                   EVAL      VATLNP = VABLNP
044500160908     C                   EVAL      VATAAS = VABAAS
044600160908     C                   EVAL      VATNRS = VABNRS
044700160908     C                   EVAL      VATNSP = VABNSP
044800160908     C                   WRITE     FIVAT000
044900160908     C                   ADD       1             §CTROKVT
045000160908     C                   ENDIF
045100160908     C*
045200160908     C                   ENDSR
045300160908
045400160908
045500160908     C*----------------------------------------------------*
045600160908     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE ACCESSORIO
045700160908     C*----------------------------------------------------*
045800160908     C     PREVAT        BEGSR
045900160908     C*
046000160908     C* Compongo il nome del membro da dare al FIVATWWR
046100160908     C                   eval      parmbr = vlrhdl
046200160908     C                   movel     'M'           parmbr
046300160908     C                   eval      parccm = %subst(vlrKSC:2:7)
046400160908     C                   eval      paropz = '1'
046500160908     C*
046600160908     C* Effettuo la chiamata al CLLE preposto
046700160908     C                   call(e)   'TITVVTC'
046800160908     C                   parm                    parccm
046900160908     C                   parm                    parmbr
047000160908     C                   parm                    paropz
047100160908     C*
047200160908     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
047300160908     C                   if        %error
047400160908     C                   movel     '1'           chkcall
047500160908     C                   else
047600160908     C                   movel     '0'           chkcall
047700160908     C                   endif
047800160908     C*
047900160908     C                   ENDSR
048000160908
048100160908
048200160908      /TITLE Invio dei dati al punto operativo.
048300160908     C     invio         BEGSR
048400160908     C*
048500160908     C* 1° invio FIVAT
048600160908     C                   reset                   dscmz
048700160908     C                   move      vlrpoi        cmzdst
048800160908     C                   eval      cmzfld = 'FIVATWWR'
048900160908     C                   eval      cmzmbd = vlrhdl
049000160908     C                   eval      %subst(cmzmbd:1:1) = 'M'
049100160908     C***                if        prmfir = *blanks
049200160908     C                   eval      cmzfla = 'FIVAT00F'
049300160908     C                   eval      cmzmba = 'FIVAT00F'
049400160908     C***                else
049500160908     C***                eval      cmzfla = prmfir
049600160908     C***                eval      cmzmba = prmfir
049700160908     C***                endif
049800160908     C                   eval      cmznrr = *zeros
049900160908     C                   move      §ctrokvt      cmznrr
050000160908     C                   eval      cmzlba = vlrfl1
050100160908     C                   call(e)   'TIS711C'
050200160908     C                   parm                    dscmz
050300160908     C                   parm      *blanks       esito
050400160908     C                   if        %error
050500160908     C                             or cmzerr = '1'
050600160908     C                             or esito  = '1'
050700160908     C                   eval      wrkesito = '3'
050800160908     C                   else
050900160908     C*
051000160908     C* 2° invio FIVAB
051100160908     C                   reset                   dscmz
051200160908     C                   move      vlrpoi        cmzdst
051300160908     C                   eval      cmzfld = vlrfou
051400160908     C                   eval      cmzmbd = vlrhdl
051500160908     C                   eval      %subst(cmzmbd:1:1) = 'M'
051600160908     C***                if        prmfir = *blanks
051700160908     C                   eval      cmzfla = 'FIVAB00F'
051800160908     C                   eval      cmzmba = 'FIVAB00F'
051900160908     C***                else
052000160908     C***                eval      cmzfla = prmfir
052100160908     C***                eval      cmzmba = prmfir
052200160908     C***                endif
052300160908     C                   eval      cmznrr = *zeros
052400160908     C                   move      §ctrokvb      cmznrr
052500160908     C                   eval      cmzlba = vlrfl1
052600160908     C                   call(e)   'TIS711C'
052700160908     C                   parm                    dscmz
052800160908     C                   parm      *blanks       esito
052900160908     C                   if        %error
053000160908     C                             or cmzerr = '1'
053100160908     C                             or esito  = '1'
053200160908     C                   eval      wrkesito = '3'
053300160908     C                   endif
053400160908     C                   endif
053500160908     C*
053600160908     C                   ENDSR
053700160908     C***
053800010601
053900010601
054000010330     C*----------------------------------------------------*
054100030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
054200010330     C*----------------------------------------------------*
054300010330     C     INZVAR        BEGSR
054400010330     C*
054500030822     C* Inizializzo variabili di wrk
054600160902     C                   Z-ADD     *zeros        Num5_0            5 0
054700030822     C*
054800030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
054900160908     C                   CLEAR                   FIVAB000
055000160908     C                   CLEAR                   FIVAT000
055100170322     C                   CLEAR                   wVABRMA_1
055200170322     C                   CLEAR                   wVABRMA_2
055300160908     C                   CLEAR                   wVABNOT
055400160902     C                   CLEAR                   wVATNOT_A
055500160902     C                   CLEAR                   wVATNOT_B
055600160902     C                   CLEAR                   wVATNOT_E
055700150420     C                   CLEAR                   wVATNOT_I
055800160908     C                   CLEAR                   wVATNOT_J
055900150420     C                   CLEAR                   wVATNOT_S
056000030822     C                   CLEAR                   SkSplitCSV
056100160902     C                   CLEAR                   wFlgCAS
056200030822     C                   EVAL      i = 1
056300030822     C*
056400030822     C* Reimposto i valori di default
056500030822     C                   EXSR      DEFCAM
056600030822     C*
056700010330     C                   ENDSR
056800160902
056900160902
057000010330     C*----------------------------------------------------*
057100030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
057200010330     C*----------------------------------------------------*
057300010330     C     DEFCAM        BEGSR
057400110706     C*
057500160906     C                   SETOFF                                       7172
057600030822     C*
057700030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
057800030715     C* e delimitatore testo.
057900030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
058000170315     C*
058100170315     C* Verifico eventuali valori particolari per il carattere separatore campo
058200170315     C                   IF        CharCSV = '5'
058300170315     C                   EVAL      CharCSV = x'05'
058400170315     C                   ENDIF
058500170315     C*
058600030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
058700030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
058800150420     C                   IF        %subst(vlrppt:5:1) <> *blanks
058900150420     C                   EVAL      pptCSV = %subst(vlrppt:5:1)
059000150420     C                   ELSE
059100150420     C                   EVAL      pptCSV = ';'
059200150420     C                   ENDIF
059300150420     C                   IF        %subst(vlrppt:6:1) <> *blanks
059400150420     C                   EVAL      pptCHR = %subst(vlrppt:6:1)
059500150420     C                   ELSE
059600150420     C                   EVAL      pptCHR = ' '
059700150420     C                   ENDIF
059800090403     C*
059900090403     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
060000160902     C                   EVAL      CharSOS = CharNUM
060100030822     C*
060200030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
060300030715     C                   IF        %subst(vlrppt:1:1) = 'S'
060400160902     C                   SETON                                        50        * OUT => Download
060500030715     C                   ELSE
060600160902     C                   SETOFF                                       50        * OUT => File WW
060700030715     C                   ENDIF
060800030822     C*
060900030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
061000030822     C                   EVAL      posDaDft = 1
061100030822     C                   EVAL      posADft  = 0
061200030822     C                   EVAL      wGiroDft = 0
061300030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
061400030822     C                             posDaDft > 0
061500030822     C*
061600030822     C* Gestisco il 1° giro
061700030822     C                   IF        wGiroDft = 0
061800030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
061900030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
062000030822     C* Incremento il contatore dei "giri"
062100030822     C                   EVAL      wGiroDft = 1
062200030822     C                   ELSE
062300030822     C                   EVAL      posDaDft = posADft
062400030822     C                   ENDIF
062500030822     C* Eseguo lo scan x trovare la fine del campo corrente
062600030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
062700030822     C*
062800030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
062900030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
063000030822     C                   IF        posDaDft > 0 AND
063100030822     C                             posADft  > 0
063200110706     C* RMN
063300110706     C                   IF        %subst(
063400110706     C                             %subst(vlrppt:posDaDft+1:
063500110706     C                             posADft-posDaDft-1):1:3)
063600110706     C                             = 'RMN'
063700110706     C                   IF        %trim(%subst(
063800110706     C                             %subst(vlrppt:posDaDft+1:
063900110706     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064000110706     C                             = '*NSP'
064100160902     C                   SETON                                        71
064200110706     C                   ENDIF
064300110706     C                   ENDIF
064400060331     C* FGS
064500060331     C                   IF        %subst(
064600060331     C                             %subst(vlrppt:posDaDft+1:
064700060331     C                             posADft-posDaDft-1):1:3)
064800060331     C                             = 'FGS'
064900060331     C                   EVAL      PiStr=%trim(%subst(
065000060331     C                             %subst(vlrppt:posDaDft+1:
065100060331     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065200060331     C                   EXSR      CHKNUM
065300060331     C                   IF        PiInt=*on
065400060331     C                   Z-ADD     PiVal         VABFGS
065500060331     C                   ENDIF
065600060331     C                   ENDIF
065700030822     C* NCL
065800030822     C                   IF        %subst(
065900030822     C                             %subst(vlrppt:posDaDft+1:
066000030822     C                             posADft-posDaDft-1):1:3)
066100030822     C                             = 'NCL'
066200030822     C                   EVAL      PiStr=%trim(%subst(
066300030822     C                             %subst(vlrppt:posDaDft+1:
066400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066500030822     C                   EXSR      CHKNUM
066600030822     C                   IF        PiInt=*on
066700030822     C                   Z-ADD     PiVal         VABNCL
066800030822     C                   ENDIF
066900030822     C                   ENDIF
067000030822     C* CCM
067100030822     C                   IF        %subst(
067200030822     C                             %subst(vlrppt:posDaDft+1:
067300030822     C                             posADft-posDaDft-1):1:3)
067400030822     C                             = 'CCM'
067500030822     C                   EVAL      PiStr=%trim(%subst(
067600030822     C                             %subst(vlrppt:posDaDft+1:
067700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067800030822     C                   EXSR      CHKNUM
067900030822     C                   IF        PiInt=*on
068000030822     C                   Z-ADD     PiVal         VABCCM
068100030822     C                   ENDIF
068200030822     C                   ENDIF
068300030822     C* LNP
068400030822     C                   IF        %subst(
068500030822     C                             %subst(vlrppt:posDaDft+1:
068600030822     C                             posADft-posDaDft-1):1:3)
068700030822     C                             = 'LNP'
068800030822     C                   EVAL      PiStr=%trim(%subst(
068900030822     C                             %subst(vlrppt:posDaDft+1:
069000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069100030822     C                   EXSR      CHKNUM
069200030822     C                   IF        PiInt=*on
069300030822     C                   Z-ADD     PiVal         VABLNP
069400030822     C                   ENDIF
069500030822     C                   ENDIF
069600030822     C* NRS
069700030822     C                   IF        %subst(
069800030822     C                             %subst(vlrppt:posDaDft+1:
069900030822     C                             posADft-posDaDft-1):1:3)
070000030822     C                             = 'NRS'
070100030822     C                   EVAL      PiStr=%trim(%subst(
070200030822     C                             %subst(vlrppt:posDaDft+1:
070300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070400030822     C                   EXSR      CHKNUM
070500030822     C                   IF        PiInt=*on
070600030822     C                   Z-ADD     PiVal         VABNRS
070700030822     C                   ENDIF
070800030822     C                   ENDIF
070900160906     C* CTR
071000030822     C                   IF        %subst(
071100030822     C                             %subst(vlrppt:posDaDft+1:
071200030822     C                             posADft-posDaDft-1):1:3)
071300030822     C                             = 'CTR'
071400030822     C                   EVAL      PiStr=%trim(%subst(
071500030822     C                             %subst(vlrppt:posDaDft+1:
071600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071700030822     C                   EXSR      CHKNUM
071800030822     C                   IF        PiInt=*on
071900160906     C                   Z-ADD     PiVal         VABCTR
072000030822     C                   ENDIF
072100030822     C                   ENDIF
072200160906     C* CTE (ovvero CTR per Estero)
072300160906     C                   IF        %subst(
072400160906     C                             %subst(vlrppt:posDaDft+1:
072500160906     C                             posADft-posDaDft-1):1:3)
072600160906     C                             = 'CTE'
072700160906     C                   EVAL      PiStr=%trim(%subst(
072800160906     C                             %subst(vlrppt:posDaDft+1:
072900160906     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073000160906     C                   EXSR      CHKNUM
073100160906     C                   IF        PiInt=*on
073200160906     C                   Z-ADD     PiVal         wVABCTR_E
073300160906     C                   SETON                                        72
073400160906     C                   ENDIF
073500160906     C                   ENDIF
073600030822     C* PKB
073700030822     C                   IF        %subst(
073800030822     C                             %subst(vlrppt:posDaDft+1:
073900030822     C                             posADft-posDaDft-1):1:3)
074000030822     C                             = 'PKB'
074100030822     C                   EVAL      PiStr=%trim(%subst(
074200030822     C                             %subst(vlrppt:posDaDft+1:
074300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074400030822     C                   EXSR      CHKNUM
074500030822     C                   IF        PiNum=*on
074600030822     C                   Z-ADD     PiVal         VABPKB
074700030822     C                   ENDIF
074800030822     C                   ENDIF
074900030822     C* VLB
075000030822     C                   IF        %subst(
075100030822     C                             %subst(vlrppt:posDaDft+1:
075200030822     C                             posADft-posDaDft-1):1:3)
075300030822     C                             = 'VLB'
075400030822     C                   EVAL      PiStr=%trim(%subst(
075500030822     C                             %subst(vlrppt:posDaDft+1:
075600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075700030822     C                   EXSR      CHKNUM
075800030822     C                   IF        PiNum=*on
075900030822     C                   Z-ADD     PiVal         VABVLB
076000030822     C                   ENDIF
076100030822     C                   ENDIF
076200030822     C* QFT
076300030822     C                   IF        %subst(
076400030822     C                             %subst(vlrppt:posDaDft+1:
076500030822     C                             posADft-posDaDft-1):1:3)
076600030822     C                             = 'QFT'
076700030822     C                   EVAL      PiStr=%trim(%subst(
076800030822     C                             %subst(vlrppt:posDaDft+1:
076900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077000030822     C                   EXSR      CHKNUM
077100030822     C                   IF        PiNum=*on
077200030822     C                   Z-ADD     PiVal         VABQFT
077300030822     C                   ENDIF
077400030822     C                   ENDIF
077500170316     C* ATB
077600170316     C                   IF        %subst(
077700170316     C                             %subst(vlrppt:posDaDft+1:
077800170316     C                             posADft-posDaDft-1):1:3)
077900170316     C                             = 'ATB'
078000170316     C                   EVAL      VABATB=%trim(%subst(
078100170316     C                             %subst(vlrppt:posDaDft+1:
078200170316     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078300170316     C                   ENDIF
078400030822     C* CBO
078500030822     C                   IF        %subst(
078600030822     C                             %subst(vlrppt:posDaDft+1:
078700030822     C                             posADft-posDaDft-1):1:3)
078800030822     C                             = 'CBO'
078900030822     C                   EVAL      VABCBO=%trim(%subst(
079000030822     C                             %subst(vlrppt:posDaDft+1:
079100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079200030822     C                   ENDIF
079300030822     C* TSP
079400030822     C                   IF        %subst(
079500030822     C                             %subst(vlrppt:posDaDft+1:
079600030822     C                             posADft-posDaDft-1):1:3)
079700030822     C                             = 'TSP'
079800030822     C                   EVAL      VABTSP=%trim(%subst(
079900030822     C                             %subst(vlrppt:posDaDft+1:
080000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080100030822     C                   ENDIF
080200030822     C* VAS
080300030822     C                   IF        %subst(
080400030822     C                             %subst(vlrppt:posDaDft+1:
080500030822     C                             posADft-posDaDft-1):1:3)
080600030822     C                             = 'VAS'
080700030822     C                   EVAL      VABVAS=%trim(%subst(
080800030822     C                             %subst(vlrppt:posDaDft+1:
080900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081000030822     C                   ENDIF
081100030822     C* VCA
081200030822     C                   IF        %subst(
081300030822     C                             %subst(vlrppt:posDaDft+1:
081400030822     C                             posADft-posDaDft-1):1:3)
081500030822     C                             = 'VCA'
081600030822     C                   EVAL      VABVCA=%trim(%subst(
081700030822     C                             %subst(vlrppt:posDaDft+1:
081800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081900030822     C                   ENDIF
082000030822     C* GCA
082100030822     C                   IF        %subst(
082200030822     C                             %subst(vlrppt:posDaDft+1:
082300030822     C                             posADft-posDaDft-1):1:3)
082400030822     C                             = 'GCA'
082500030822     C                   EVAL      VABGCA=%trim(%subst(
082600030822     C                             %subst(vlrppt:posDaDft+1:
082700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082800030822     C                   ENDIF
082900030822     C* CTM
083000030822     C                   IF        %subst(
083100030822     C                             %subst(vlrppt:posDaDft+1:
083200030822     C                             posADft-posDaDft-1):1:3)
083300030822     C                             = 'CTM'
083400030822     C                   EVAL      VABCTM=%trim(%subst(
083500030822     C                             %subst(vlrppt:posDaDft+1:
083600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
083700030822     C                   ENDIF
083800030822     C* FFD
083900030822     C                   IF        %subst(
084000030822     C                             %subst(vlrppt:posDaDft+1:
084100030822     C                             posADft-posDaDft-1):1:3)
084200030822     C                             = 'FFD'
084300030822     C                   EVAL      VABFFD=%trim(%subst(
084400030822     C                             %subst(vlrppt:posDaDft+1:
084500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
084600030822     C                   ENDIF
084700030822     C* VAD
084800030822     C                   IF        %subst(
084900030822     C                             %subst(vlrppt:posDaDft+1:
085000030822     C                             posADft-posDaDft-1):1:3)
085100030822     C                             = 'VAD'
085200030822     C                   EVAL      VABVAD=%trim(%subst(
085300030822     C                             %subst(vlrppt:posDaDft+1:
085400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
085500030822     C                   ENDIF
085600030822     C* GMA
085700030822     C                   IF        %subst(
085800030822     C                             %subst(vlrppt:posDaDft+1:
085900030822     C                             posADft-posDaDft-1):1:3)
086000030822     C                             = 'GMA'
086100030822     C                   EVAL      VABGMA=%trim(%subst(
086200030822     C                             %subst(vlrppt:posDaDft+1:
086300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
086400030822     C                   ENDIF
086500030822     C* GGA
086600030822     C                   IF        %subst(
086700030822     C                             %subst(vlrppt:posDaDft+1:
086800030822     C                             posADft-posDaDft-1):1:3)
086900030822     C                             = 'GGA'
087000030822     C                   EVAL      VABGGA=%trim(%subst(
087100030822     C                             %subst(vlrppt:posDaDft+1:
087200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
087300030822     C                   ENDIF
087400030822     C* GVA
087500030822     C                   IF        %subst(
087600030822     C                             %subst(vlrppt:posDaDft+1:
087700030822     C                             posADft-posDaDft-1):1:3)
087800030822     C                             = 'GVA'
087900030822     C                   EVAL      VABGVA=%trim(%subst(
088000030822     C                             %subst(vlrppt:posDaDft+1:
088100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
088200030822     C                   ENDIF
088300030822     C* TC1
088400030822     C                   IF        %subst(
088500030822     C                             %subst(vlrppt:posDaDft+1:
088600030822     C                             posADft-posDaDft-1):1:3)
088700030822     C                             = 'TC1'
088800030822     C                   EVAL      VABTC1=%trim(%subst(
088900030822     C                             %subst(vlrppt:posDaDft+1:
089000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
089100030822     C                   ENDIF
089200030822     C* TC2
089300030822     C                   IF        %subst(
089400030822     C                             %subst(vlrppt:posDaDft+1:
089500030822     C                             posADft-posDaDft-1):1:3)
089600030822     C                             = 'TC2'
089700030822     C                   EVAL      VABTC2=%trim(%subst(
089800030822     C                             %subst(vlrppt:posDaDft+1:
089900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
090000030822     C                   ENDIF
090100030822     C* ...
090200030822     C                   ENDIF
090300030822     C                   ENDDO
090400020204     C*
090500010330     C                   ENDSR
090600010607     C*----------------------------------------------------*
090700021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
090800010607     C*----------------------------------------------------*
090900010607     C     IMPVAB        BEGSR
091000010607     C*
091100010607     C                   SETOFF                                       3132
091200030515     C*
091300010607     C                   MOVEL     datcor        VABAAS
091400010607     C                   MOVE      datcor        VABMGS
091500160902     C*
091600160902     C* Dopo aver splittato nei vari campi eseguo 1 prima "normalizzazione" x eliminare
091700160902     C* i caratteri di delimitazione testo e gli apici
091800160902     C                   EVAL      SkSplitCSV = SkSplitCSV_5
091900160902     C                   EVAL      i = 1
092000160902     C                   DOW       i < %elem(SkSplitCSV)
092100160902     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
092200160902     C     '''':' '      XLATE     SkSplitCSV(i) SkSplitCSV(i)
092300160902     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
092400160902     C                   EVAL      i = i + 1
092500160902     C                   ENDDO
092600010607     C*
092700030715     C**********
092800160902     C* Valorizzo la lista delle INTESTAZIONI colonne cliente
092900030715     C**********
093000030715     C                   IF        wGiro = *zeros
093100160902     C*
093200160902     C                   EVAL      wGiro = 1
093300160902     C                   EVAL      SkSplitFLD = SkSplitCSV
093400160902     C*
093500160902     C                   ELSE
093600160902     C*
093700160902     C**********
093800160902     C* Elaboro le righe di DATI cliente
093900160902     C**********
094000160902     C                   EVAL      wGiro = 2
094100030715     C*
094200030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
094300030715     C                   EVAL      i = 1
094400030715     C                   DOW       i < %elem(SkSplitFLD)
094500030715     C*
094600030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
094700030715     C                   IF        SkSplitFLD(i) <> *blanks AND
094800030715     C                             SkSplitCSV(i) <> *blanks
094900130624     C*** ATB
095000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
095100150420     C                   EVAL      VABATB = SkSplitCSV(i)
095200030721     C                   ENDIF
095300030715     C*** RSD
095400160902     C                   IF        %trim(SkSplitFLD(i)) =
095500160908     C                             'recipient-name'
095600030715     C                   EVAL      VABRSD = SkSplitCSV(i)
095700030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
095800030715     C     '@':'A'       XLATE     VABRSD        VABRSD
095900030715     C* ==
096000030715     C                   ENDIF
096100030721     C*** RD2
096200160902     C                   IF        %trim(SkSplitFLD(i)) =
096300160908     C                             'ship-address-2'
096400160902     C                   EVAL      VABRD2 = SkSplitCSV(i)
096500160902     C                   ENDIF
096600030715     C*** IND
096700160902     C                   IF        %trim(SkSplitFLD(i)) =
096800160908     C                             'ship-address-1'
096900030715     C                   EVAL      VABIND = SkSplitCSV(i)
097000030715     C                   ENDIF
097100030715     C*** LOD
097200160902     C                   IF        %trim(SkSplitFLD(i)) =
097300160908     C                             'ship-city'
097400030715     C                   EVAL      VABLOD = SkSplitCSV(i)
097500030715     C                   ENDIF
097600030715     C*** PRD
097700160902     C                   IF        %trim(SkSplitFLD(i)) =
097800160908     C                             'VABPRD'
097900030715     C                   EVAL      VABPRD = SkSplitCSV(i)
098000030715     C                   ENDIF
098100160902     C*** CAD
098200160902     C                   IF        %trim(SkSplitFLD(i)) =
098300160908     C                             'ship-postal-code'
098400160902     C                   EVAL      VABCAD = SkSplitCSV(i)
098500160902     C                   ENDIF
098600030721     C*** NZD
098700160902     C                   IF        %trim(SkSplitFLD(i)) =
098800160908     C                             'ship-country'
098900170315     C                   EVAL      VABNZD = SkSplitCSV(i)
099000170316     C                   Z-ADD     1             jNAZ
099100170316     C     VABNZD        LOOKUP    skNAZISO(jNAZ)                         13
099200170316     C                   IF        %found
099300170316     C                   EVAL      VABNZD = skNAZBAR(jNAZ)
099400170316     C                   ENDIF
099500030721     C                   ENDIF
099600170522     C*** RMA - 2
099700160908     C                   IF        %trim(SkSplitFLD(i)) = 'order-id'
099800170522     C                   EVAL      wVABRMA_2 = SkSplitCSV(i)
099900160908     C                   EVAL      wVABNOT = SkSplitCSV(i)
100000150515     C                   ENDIF
100100030715     C*** NOT
100200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
100300030715     C                   EVAL      VABNOT = SkSplitCSV(i)
100400030715     C                   ENDIF
100500030715     C*** NT2
100600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
100700030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
100800030715     C                   ENDIF
100900030715     C*** VCA
101000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
101100030715     C                   EVAL      VABVCA = SkSplitCSV(i)
101200030715     C                   ENDIF
101300030715     C*** VAS
101400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
101500030715     C                   EVAL      VABVAS = SkSplitCSV(i)
101600030715     C                   ENDIF
101700030715     C*** FFD
101800160902     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
101900030715     C                   EVAL      VABFFD = SkSplitCSV(i)
102000030715     C                   ENDIF
102100160902     C*** CBO
102200160902     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
102300160902     C                   EVAL      VABCBO = SkSplitCSV(i)
102400150420     C                   ENDIF
102500030721     C*** GC1
102600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
102700150420     C                   IF        %len(%trim(SkSplitCSV(i))) = 1
102800150420     C                   MOVEL     *blanks       wGC               1
102900150420     C                   EVAL      wGC = %trim(SkSplitCSV(i))
103000150420     C                   IF        SkSplitCSV(i) = 'M' OR
103100150420     C                             SkSplitCSV(i) = 'P'
103200150420     C                   MOVE(P)   wGC           VABGC1
103300150420     C                   ELSE
103400150420     C                   MOVEL(P)  wGC           VABGC1
103500150420     C                   ENDIF
103600150420     C                   ENDIF
103700030721     C                   ENDIF
103800030721     C*** GC2
103900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
104000120314     C                   IF        %len(%trim(SkSplitCSV(i))) = 1
104100120314     C                   MOVEL     *blanks       wGC               1
104200120314     C                   EVAL      wGC = %trim(SkSplitCSV(i))
104300120314     C                   IF        SkSplitCSV(i) = 'M' OR
104400120314     C                             SkSplitCSV(i) = 'P'
104500120314     C                   MOVE(P)   wGC           VABGC2
104600120314     C                   ELSE
104700120314     C                   MOVEL(P)  wGC           VABGC2
104800120314     C                   ENDIF
104900120314     C                   ENDIF
105000030721     C                   ENDIF
105100030721     C*** TSP
105200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
105300030721     C                   EVAL      VABTSP = SkSplitCSV(i)
105400030721     C                   ENDIF
105500030721     C*** NAS
105600160902     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
105700030721     C                   EVAL      VABNAS = SkSplitCSV(i)
105800030721     C                   ENDIF
105900030721     C*** TIC
106000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
106100030721     C                   EVAL      VABTIC = SkSplitCSV(i)
106200030721     C                   ENDIF
106300030721     C*** GCA
106400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
106500030721     C                   EVAL      VABGCA = SkSplitCSV(i)
106600030721     C                   ENDIF
106700030721     C*** XCO
106800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
106900030721     C                   EVAL      VABXCO = SkSplitCSV(i)
107000030721     C                   ENDIF
107100030721     C*** CTM
107200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
107300030721     C                   EVAL      VABCTM = SkSplitCSV(i)
107400030721     C                   ENDIF
107500030721     C*** TCR
107600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
107700030721     C                   EVAL      VABTCR = SkSplitCSV(i)
107800030721     C                   ENDIF
107900030721     C*** CTS
108000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
108100030721     C                   EVAL      VABCTS = SkSplitCSV(i)
108200030721     C                   ENDIF
108300030721     C*** FTM
108400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
108500030721     C                   EVAL      VABFTM = SkSplitCSV(i)
108600030721     C                   ENDIF
108700030721     C*** VAD
108800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
108900030721     C                   EVAL      VABVAD = SkSplitCSV(i)
109000030721     C                   ENDIF
109100030721     C*** GMA
109200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
109300030721     C                   EVAL      VABGMA = SkSplitCSV(i)
109400030721     C                   ENDIF
109500030721     C*** GGA
109600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
109700030721     C                   EVAL      VABGGA = SkSplitCSV(i)
109800030721     C                   ENDIF
109900030721     C*** GVA
110000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
110100030721     C                   EVAL      VABGVA = SkSplitCSV(i)
110200030721     C                   ENDIF
110300030721     C*** TC1
110400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
110500030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
110600030721     C                   ENDIF
110700030721     C*** TC2
110800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
110900030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
111000030721     C                   ENDIF
111100030721     C*** SCL
111200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
111300030721     C                   EVAL      VABSCL = SkSplitCSV(i)
111400030721     C                   ENDIF
111500030721     C*** RMO
111600160902     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
111700030721     C                   EVAL      VABRMO = SkSplitCSV(i)
111800030721     C                   ENDIF
111900160902     C*** CMO
112000160902     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
112100160902     C                   EVAL      VABCMO = SkSplitCSV(i)
112200160902     C                   ENDIF
112300030721     C*** NMO
112400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
112500030721     C                   EVAL      VABNMO = SkSplitCSV(i)
112600030721     C                   ENDIF
112700160908     C*** NOT_A
112800160908     C                   IF        %trim(SkSplitFLD(i)) =
112900160908     C                             'buyer-name'
113000160908     C                   EVAL      wVATNOT_A = SkSplitCSV(i)
113100160908     C                   ENDIF
113200160902     C*** NOT_B
113300160902     C                   IF        %trim(SkSplitFLD(i)) =
113400160908     C                             'buyer-phone-number'
113500160902     C                   EVAL      wVATNOT_B = SkSplitCSV(i)
113600160902     C                   ENDIF
113700150420     C*** NOT_I
113800160902     C                   IF        %trim(SkSplitFLD(i)) =
113900160908     C                             'buyer-email'
114000150420     C                   EVAL      wVATNOT_I = SkSplitCSV(i)
114100150420     C                   ENDIF
114200150420     C*** NOT_S
114300170315     C                   IF        %trim(SkSplitFLD(i)) =
114400170315     C                             'ship-phone-number'
114500150420     C                   EVAL      wVATNOT_S = SkSplitCSV(i)
114600150420     C                   ENDIF
114700030721     C*
114800070705     C* Reperisco quindi i campi numerici...
114900030721     C*** RMN
115000160908     C                   IF        %trim(SkSplitFLD(i)) =
115100160908     C                             'order-item-id'
115200170522     C                   EVAL      wVABRMA_1 = SkSplitCSV(i)
115300030721     C                   EVAL      PiStr=SkSplitCSV(i)
115400030721     C                   EXSR      CHKNUM
115500030721     C                   IF        PiInt=*on
115600030721     C                   Z-ADD     PiVal         VABRMN
115700030721     C                   ELSE
115800030721     C                   SETON                                        32
115900030721     C                   EVAL      VABRMN = 1
116000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116100030721     C                             + ' ' + 'VABRMN'
116200030721     C                   ENDIF
116300030721     C                   ENDIF
116400030721     C*** NSP
116500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
116600030721     C                   EVAL      PiStr=SkSplitCSV(i)
116700030721     C                   EXSR      CHKNUM
116800100517     C                   IF        PiInt=*on
116900030721     C                   Z-ADD     PiVal         VABNSP
117000030721     C                   ELSE
117100030721     C                   SETON                                        32
117200030721     C                   EVAL      VABNSP = *zeros
117300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117400030721     C                             + ' ' + 'VABNSP'
117500030721     C                   ENDIF
117600030721     C                   ENDIF
117700160902     C*** NCL
117800160902     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
117900030721     C                   EVAL      PiStr=SkSplitCSV(i)
118000030721     C                   EXSR      CHKNUM
118100030721     C                   IF        PiInt=*on
118200030721     C                   Z-ADD     PiVal         VABNCL
118300030721     C                   ELSE
118400030721     C                   SETON                                        32
118500030721     C                   EVAL      VABNCL = *zeros
118600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118700030721     C                             + ' ' + 'VABNCL'
118800030721     C                   ENDIF
118900030721     C                   ENDIF
119000030721     C*** PKB
119100160902     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
119200030721     C                   EVAL      PiStr=SkSplitCSV(i)
119300030721     C                   EXSR      CHKNUM
119400030721     C                   IF        PiNum=*on
119500030721     C                   Z-ADD     PiVal         VABPKB
119600030721     C                   ELSE
119700030721     C                   SETON                                        32
119800030721     C                   EVAL      VABPKB = *zeros
119900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120000030721     C                             + ' ' + 'VABPKB'
120100030721     C                   ENDIF
120200030721     C                   ENDIF
120300160908     C*** CAS
120400160908     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
120500160908     C                   IF        SkSplitCSV(i) <> *blanks
120600160908     C                   EVAL      wFlgCAS = 1
120700160908     C                   ENDIF
120800160902     C                   EVAL      PiStr=SkSplitCSV(i)
120900160902     C                   EXSR      CHKNUM
121000160902     C                   IF        PiNum=*on
121100160902     C                   Z-ADD     PiVal         VABCAS
121200160902     C                   MOVEL     'EUR'         VABVCA
121300160902     C                   ELSE
121400160902     C                   SETON                                        32
121500160902     C                   EVAL      VABCAS = *zeros
121600160902     C                   EVAL      vinmsg = %trimr(vinmsg)
121700160902     C                             + ' ' + 'VABCAS'
121800160902     C                   ENDIF
121900160902     C                   ENDIF
122000030721     C*** CCM
122100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
122200030721     C                   EVAL      PiStr=SkSplitCSV(i)
122300030721     C                   EXSR      CHKNUM
122400030721     C                   IF        PiInt=*on      AND
122500030721     C                             PiVal<=9999999 AND
122600030721     C                             PiVal>*zeros
122700030721     C                   Z-ADD     PiVal         VABCCM
122800030721     C                   ELSE
122900030721     C                   SETON                                        32
123000030721     C                   EVAL      VABCCM = *zeros
123100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123200030721     C                             + ' ' + 'VABCCM'
123300030721     C                   ENDIF
123400030721     C                   ENDIF
123500030721     C*** LNP
123600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
123700030721     C                   EVAL      PiStr=SkSplitCSV(i)
123800030721     C                   EXSR      CHKNUM
123900030721     C                   IF        PiInt=*on    AND
124000030721     C                             PiVal<=999   AND
124100030721     C                             PiVal>*zeros
124200030721     C                   Z-ADD     PiVal         VABLNP
124300030721     C                   ELSE
124400030721     C                   SETON                                        32
124500030721     C                   EVAL      VABLNP = *zeros
124600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124700030721     C                             + ' ' + 'VABLNP'
124800030721     C                   ENDIF
124900030721     C                   ENDIF
125000030721     C*** NRS
125100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
125200030721     C                   EVAL      PiStr=SkSplitCSV(i)
125300030721     C                   EXSR      CHKNUM
125400030721     C                   IF        PiInt=*on AND
125500030721     C                             PiVal<=99
125600030721     C                   Z-ADD     PiVal         VABNRS
125700030721     C                   ELSE
125800030721     C                   SETON                                        32
125900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126000030721     C                             + ' ' + 'VABNRS'
126100030721     C                   ENDIF
126200030721     C                   ENDIF
126300030721     C*** LNA
126400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
126500120314     C                   IF        %trim(SkSplitCSV(i)) <> *zeros  AND
126600120314     C                             %trim(SkSplitCSV(i)) <> '0'     AND
126700120314     C                             %trim(SkSplitCSV(i)) <> '000'   AND
126800120314     C                             %trim(SkSplitCSV(i)) <> *blanks
126900030721     C                   EVAL      PiStr=SkSplitCSV(i)
127000030721     C                   EXSR      CHKNUM
127100030721     C                   IF        PiInt=*on    AND
127200030721     C                             PiVal<=999   AND
127300030721     C                             PiVal>*zeros
127400030721     C                   Z-ADD     PiVal         VABLNA
127500030721     C                   ELSE
127600030721     C                   SETON                                        32
127700030721     C                   EVAL      VABLNA = *zeros
127800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127900030721     C                             + ' ' + 'VABLNA'
128000120314     C                   ENDIF
128100030721     C                   ENDIF
128200030721     C                   ENDIF
128300030721     C*** CTR
128400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
128500030721     C                   EVAL      PiStr=SkSplitCSV(i)
128600030721     C                   EXSR      CHKNUM
128700030721     C                   IF        PiInt=*on  AND
128800030721     C                             PiVal<=999
128900030721     C                   Z-ADD     PiVal         VABCTR
129000030721     C                   ELSE
129100030721     C                   SETON                                        32
129200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129300030721     C                             + ' ' + 'VABCTR'
129400030721     C                   ENDIF
129500030721     C                   ENDIF
129600030721     C*** IAS
129700160902     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
129800030721     C                   EVAL      PiStr=SkSplitCSV(i)
129900030721     C                   EXSR      CHKNUM
130000030721     C                   IF        PiNum=*on
130100030721     C                   Z-ADD     PiVal         VABIAS
130200030721     C                   ELSE
130300030721     C                   SETON                                        32
130400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130500030721     C                             + ' ' + 'VABIAS'
130600030721     C                   ENDIF
130700030721     C                   ENDIF
130800030721     C*** VLB
130900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
131000030721     C                   EVAL      PiStr=SkSplitCSV(i)
131100030721     C                   EXSR      CHKNUM
131200030721     C                   IF        PiNum=*on
131300030721     C                   Z-ADD     PiVal         VABVLB
131400030721     C                   ELSE
131500030721     C                   SETON                                        32
131600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131700030721     C                             + ' ' + 'VABVLB'
131800030721     C                   ENDIF
131900030721     C                   ENDIF
132000030721     C*** QFT
132100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
132200030721     C                   EVAL      PiStr=SkSplitCSV(i)
132300030721     C                   EXSR      CHKNUM
132400030721     C                   IF        PiNum=*on
132500030721     C                   Z-ADD     PiVal         VABQFT
132600030721     C                   ELSE
132700030721     C                   SETON                                        32
132800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132900030721     C                             + ' ' + 'VABQFT'
133000030721     C                   ENDIF
133100030721     C                   ENDIF
133200030721     C*** NCD
133300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
133400030721     C                   EVAL      PiStr=SkSplitCSV(i)
133500030721     C                   EXSR      CHKNUM
133600030721     C                   IF        PiInt=*on      AND
133700030721     C                             PiVal<=9999999
133800030721     C                   Z-ADD     PiVal         VABNCD
133900030721     C                   ELSE
134000030721     C                   SETON                                        32
134100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134200030721     C                             + ' ' + 'VABNCD'
134300030721     C                   ENDIF
134400030721     C                   ENDIF
134500030721     C*** NCA
134600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
134700030721     C                   EVAL      PiStr=SkSplitCSV(i)
134800030721     C                   EXSR      CHKNUM
134900030721     C                   IF        PiInt=*on      AND
135000030721     C                             PiVal<=9999999
135100030721     C                   Z-ADD     PiVal         VABNCA
135200030721     C                   ELSE
135300030721     C                   SETON                                        32
135400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135500030721     C                             + ' ' + 'VABNCA'
135600030721     C                   ENDIF
135700030721     C                   ENDIF
135800030721     C*** ZNC
135900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
136000030721     C                   EVAL      PiStr=SkSplitCSV(i)
136100030721     C                   EXSR      CHKNUM
136200030721     C                   IF        PiInt=*on AND
136300030721     C                             PiVal<=99
136400030721     C                   Z-ADD     PiVal         VABZNC
136500030721     C                   ELSE
136600030721     C                   SETON                                        32
136700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136800030721     C                             + ' ' + 'VABZNC'
136900030721     C                   ENDIF
137000030721     C                   ENDIF
137100030721     C*** DCR
137200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
137300030721     C                   EVAL      PiStr=SkSplitCSV(i)
137400030721     C                   EXSR      CHKNUM
137500030721     C                   IF        PiInt=*on       AND
137600030721     C                             PiVal<=99999999
137700030721     C                   Z-ADD     PiVal         VABDCR
137800030721     C                   ELSE
137900030721     C                   SETON                                        32
138000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138100030721     C                             + ' ' + 'VABDCR'
138200030721     C                   ENDIF
138300030721     C                   ENDIF
138400030721     C*** HCR
138500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
138600030721     C                   EVAL      PiStr=SkSplitCSV(i)
138700030721     C                   EXSR      CHKNUM
138800030721     C                   IF        PiInt=*on   AND
138900030721     C                             PiVal<=9999
139000030721     C                   Z-ADD     PiVal         VABHCR
139100030721     C                   ELSE
139200030721     C                   SETON                                        32
139300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
139400030721     C                             + ' ' + 'VABHCR'
139500030721     C                   ENDIF
139600030721     C                   ENDIF
139700030721     C*** VMD
139800160902     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
139900030721     C                   EVAL      PiStr=SkSplitCSV(i)
140000030721     C                   EXSR      CHKNUM
140100030721     C                   IF        PiNum=*on
140200030721     C                   Z-ADD     PiVal         VABVMD
140300030721     C                   ELSE
140400030721     C                   SETON                                        32
140500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
140600030721     C                             + ' ' + 'VABVMD'
140700030721     C                   ENDIF
140800030721     C                   ENDIF
140900030721     C*** ANT
141000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
141100030721     C                   EVAL      PiStr=SkSplitCSV(i)
141200030721     C                   EXSR      CHKNUM
141300030721     C                   IF        PiInt=*on        AND
141400030721     C                             PiVal<=999999999
141500030721     C                   Z-ADD     PiVal         VABANT
141600030721     C                   ELSE
141700030721     C                   SETON                                        32
141800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
141900030721     C                             + ' ' + 'VABANT'
142000030721     C                   ENDIF
142100030721     C                   ENDIF
142200030715     C*
142300030715     C                   ENDIF
142400030715     C                   EVAL      i = i + 1
142500030715     C                   ENDDO
142600070705     C*
142700160902     C* Considerazioni particolari "fuori ciclo"
142800110706     C*
142900160908     C* Verifica campi "particolari" VAB
143000160908     C                   EXSR      CHKVAB
143100160908     C*
143200030715     C                   ENDIF
143300020204     C*
143400010607     C                   ENDSR
143500010607     C*----------------------------------------------------*
143600160908
143700160908
143800160908     C*----------------------------------------------------*
143900160908     C*  CONSIDERAZIONI SU CAMPI PARTICOLARI VAB
144000160908     C*----------------------------------------------------*
144100160908     C     CHKVAB        BEGSR
144200160908     C*
144300160908     C*** RMN
144400160908     C                   IF        *in71
144500160908     C                   EVAL      VABRMN = VABNSP
144600160908     C                   ENDIF
144700160908     C                   IF        VABRMN = *zeros
144800160908     C                   EVAL      VABRMN = 1
144900160908     C                   ENDIF
145000160908     C*** NZD
145100160908     C                   IF        VABNZD = 'IT'  OR
145200160908     C                             VABNZD = 'ITA' OR
145300160908     C                             VABNZD = 'Ita' OR
145400160908     C                             VABNZD = 'ita'
145500160908     C                   EVAL      VABNZD = *blanks
145600160908     C                   ENDIF
145700160908     C*** CTR
145800160908     C                   IF        *in72
145900160908     C                   IF        VABNZD <> *blanks
146000160908     C                   EVAL      VABCTR =  wVABCTR_E
146100160908     C                   ENDIF
146200160908     C                   ENDIF
146300160908     C*** CBO
146400160908     C                   IF        VABCBO = *blanks
146500160908     C                   EVAL      VABCBO = '1'
146600160908     C                   ENDIF
146700160908     C*** NCL
146800160908     C                   IF        VABNCL = *zeros
146900160908     C                   EVAL      VABNCL = 1
147000160908     C                   ENDIF
147100160908     C*** CCM
147200160908     C                   IF        VABCCM = *zeros
147300160908     C                   EVAL      PiStr=VLRKSC
147400160908     C                   EXSR      CHKNUM
147500160908     C                   IF        PiInt=*on
147600160908     C                   Z-ADD     PiVal         VABCCM
147700160908     C                   ELSE
147800160908     C                   SETON                                        31
147900160908     C                   ENDIF
148000160908     C                   ENDIF
148100160908     C*** LNP
148200160908     C                   IF        VABLNP = *zeros
148300160908     C                   IF        VLRPOI = 046 OR
148400160908     C                             VLRPOI = 789 OR
148500160908     C                             VLRPOI = 999
148600160908     C                   MOVEL     VABCCM        VABLNP
148700160908     C                   ELSE
148800160908     C                   EVAL      VABLNP = VLRPOI
148900160908     C                   ENDIF
149000160908     C                   ENDIF
149100160908     C*** LNP
149200160908     C                   IF        VABFGS = *zeros
149300160908     C                   EVAL      VABFGS = VABLNP
149400160908     C                   ENDIF
149500160908     C*** FGS / LNP
149600160908     C                   IF        VABFGS = *zeros OR
149700160908     C                             VABLNP = *zeros
149800160908     C                   SETON                                        31
149900160908     C                   ENDIF
150000160908     C*** RMA
150100160908     C                   IF        wVABRMA_1 <> *blanks
150200160908     C                   EVAL      VABRMA = wVABRMA_1
150300160908     C                   ELSE
150400160920     C                   IF        wVABRMA_2 <> *blanks
150500160908     C                   EVAL      VABRMA = wVABRMA_2
150600160908     C                   ENDIF
150700160920     C                   ENDIF
150800160908     C*** CAS
150900160908     C                   IF        wFlgCAS = *zeros
151000160908     C                   EVAL      VABCAS = *zeros
151100160908     C                   EVAL      VABVCA = *blanks
151200160908     C                   ENDIF
151300160908     C*** NOT
151400160908     C                   IF        VABNOT = *blanks AND wVABNOT <> *blanks
151500160908     C                   EVAL      VABNOT = wVABNOT
151600160908     C                   ENDIF
151700160908     C*
151800160908     C* Se provincia nn valorizzata la reperisco
151900160908     C* tramite TISI95R a seconda dei dati d instradamento presenti
152000170316     C                   IF        VABNZD = *blanks AND
152100170316     C                             VABPRD = *blanks
152200160908     C                   CLEAR                   TISI95DS
152300160908     C                   EVAL      I95TCN = '3'
152400160908     C                   Z-ADD     datcor        I95DAT
152500160908     C                   EVAL      I95NAR = VABNZD
152600160908     C                   EVAL      I95CAP = VABCAD
152700160908     C                   EVAL      I95LOC = VABLOD
152800160908     C                   CALL      'TISI95R'
152900160908     C                   PARM                    TISI95DS
153000160908     C                   EVAL      VABPRD = O95PRV
153100160908     C                   ENDIF
153200160908     C*
153300160908     C* Eseguo routine finale x considerazioni specifiche su importi/divise
153400160908     C                   EXSR      CHKIMPDIV
153500160908     C*
153600160908     C                   ENDSR
153700160908     C***
153800010601
153900020204
154000020204     C*----------------------------------------------------*
154100020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
154200020204     C*----------------------------------------------------*
154300020204     C     CHKIMPDIV     BEGSR
154400060524     C*
154500060524     C* Verifico il codice bolla in base al tipo porto e alla presenza o meno del contrassegno
154600160906     C                   IF        vabCAS  > *zeros OR
154700160906     C                             wFlgCAS > *zeros
154800060524     C                   IF        vabCBO = '1'
154900060524     C                   EVAL      vabCBO = '4'
155000060524     C                   ENDIF
155100060524     C                   IF        vabCBO = '2'
155200060524     C                   EVAL      vabCBO = '6'
155300060524     C                   ENDIF
155400060524     C                   ENDIF
155500020204     C*
155600020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
155700020204     C                   Z-ADD     *zeros        wrkDec            9 9
155800020204     C*
155900020204     C* Come prima cosa effettuo considerazioni sulla divisa
156000020204     C                   IF        vabIAS > *zeros
156100150420     C                   IF        vabVAS =  *blanks
156200150420     C                   EVAL      vabVAS =  'EUR'
156300020204     C                   ENDIF
156400020204     C                   ENDIF
156500020204     C*
156600020204     C                   IF        vabCAS > *zeros
156700150420     C                   IF        vabVCA <> *blanks
156800150420     C                   EVAL      vabVCA =  'EUR'
156900020204     C                   ENDIF
157000020204     C                   ENDIF
157100020204     C*
157200020204     C                   IF        vabVMD > *zeros
157300150420     C                   IF        vabVAD <> *blanks
157400150420     C                   EVAL      vabVAD =  'EUR'
157500020204     C                   ENDIF
157600020204     C                   ENDIF
157700020204     C*
157800020204     C                   ENDSR
157900020204     C***
158000010330
158100010330
158200010330     C*----------------------------------------------------*
158300010330     C*  CONTROLLO NUMERICITA' CAMPI
158400010330     C*----------------------------------------------------*
158500010330     C     CHKNUM        BEGSR
158600010330     C*
158700010606     C                   IF        PiDecChr = *blanks
158800030715     C                   EVAL      PiDecChr = CharNUM
158900010606     C                   ENDIF
159000010606     C*
159100010606     C                   CALL(e)   'ISNUMERIC'
159200010330     C                   PARM                    PiStr            30
159300010606     C                   PARM                    PiDecChr          1
159400010330     C                   PARM      *ZEROS        PiVal            30 9
159500010330     C                   PARM      '0'           PiInt             1
159600010330     C                   PARM      '0'           PiNum             1
159700010330     C                   IF        %error
159800010606     C                   EVAL      PiNum=*off
159900010330     C                   ENDIF
160000010330     C*
160100010330     C                   ENDSR
160200010330     C***
160300160906
160400160906
160500160906      /TITLE Valorizzazione Progressivo Applicazione
160600160906     C     calprog       begsr
160700160906     C*
160800160906     C                   movel     *blanks       dwlisv            2
160900160906     C                   movel     *all'0'       dwlprg           10
161000160906     C                   z-add     *zeros        wrkprg            8 0
161100160906     C*
161200160906     C                   eval      dwlisv = vlrisv
161300160906     C*
161400170324     C     *start        setll     tis7prgf
161500160906     C                   read(e)   tis7prgf
161600160906     C                   if        not %error
161700160906     C                   eval      dwlprg = f_tis7prgf
161800160906     C*
161900160906     C                   move(p)   dwlprg        wrkprg
162000160906     C                   add       1             wrkprg
162100160906     C                   move(p)   wrkprg        dwlprg
162200160906     C                   movel     dwlisv        dwlprg
162300160906     C*
162400160906     C                   eval      f_tis7prgf = dwlprg
162500160906     C                   update    tis7prg0
162600170324     C                   else
162700170324     C                   eval      f_tis7prgf = 'XX00000000'
162800160906     C                   endif
162900160906     C*
163000160906     C                   endsr
163100050421
163200160908
163300160908     C     *pssr         BEGSR
163400160908     C*
163500160908     C                   if        %open(tivin00r)
163600160908     C                   close     tivin00r
163700160908     C                   endif
163800160908     C                   if        %open(fivabwwr)
163900160908     C                   close     fivabwwr
164000160908     C                   endif
164100160908     C                   if        %open(fivatwwr)
164200160908     C                   close     fivatwwr
164300160908     C                   endif
164400160908     C*
164500160908     C* Effettuo la chiamata al CLLE preposto
164600160908     C  N50              call(e)   'TITVVTC'
164700160908     C                   parm                    parccm
164800160908     C                   parm                    parmbr
164900160908     C                   parm      '2'           paropz
165000160908     C*
165100160908     C                   eval      wrkesito = '2'
165200160908     C*
165300160908     C                   seton                                        LR
165400160908     C*
165500160908     C                   ENDSR     '*CANCL'
165600160908     C***
165700100419
165800100419
165900160902     C     normCSV       BEGSR
166000160902     C*
166100160902     C                   z-add     *zeros        wLenVINDTA        4 0
166200160902     C                   z-add     *zeros        wFlgVINDTA        1 0
166300160902     C                   setoff                                       80
166400160902     C*
166500160902     C                   dow       wLenVINDTA < %len(vindta)
166600160902     C                   eval      wLenVINDTA = wLenVINDTA + 1
166700160902     C                   movel     *blanks       wAlfa1            1
166800160902     C                   eval      wAlfa1 = %subst(vindta:wLenVINDTA:1)
166900160902     C*
167000160902     C* Se byte inferiore a HEX40 è un byte "non stampabile" per cui considero *blank
167100170315     C                   if        wAlfa1 <  X'40' AND
167200170315     C                             wAlfa1 <> CharCSV
167300160902     C                   eval      %subst(vindta:wLenVINDTA:1) = *blanks
167400160902     C                   endif
167500160902     C*
167600160902     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
167700160902     C                   if        wAlfa1 <> CharCSV and
167800160902     C                             wAlfa1 <> CharTXT and
167900160902     C                             wAlfa1 <> CharNUM
168000160902     C                   z-add     1             wFlgVINDTA
168100160902     C                   endif
168200160902     C*
168300160902     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
168400160902     C* separatori campi
168500160902     C                   if        CharTXT <> *blanks
168600160902     C                   if        wAlfa1 = CharTXT
168700160902     C                   if        not *in80
168800160902     C                   seton                                        80
168900160902     C                   else
169000160902     C                   setoff                                       80
169100160902     C                   endif
169200160902     C                   endif
169300160902     C                   if        wAlfa1 = CharCSV and *in80 = *on
169400160902     C                   eval      %subst(vindta:wLenVINDTA:1) = CharSOS
169500160902     C                   endif
169600160902     C                   endif
169700160902     C*
169800160902     C                   enddo
169900100419     C*
170000100419     C                   ENDSR
170100100419     C***
170200150420
170300150420
170400150420      /TITLE Scrittura record in file TIVGD00F (file VAS generico download)
170500160902     C     wriVAB_VGD    BEGSR
170600160909     C*
170700160909     C* Verifica campi "particolari" VAB
170800160909     C                   exsr      repNSP
170900160909     C*
171000160909     C                   setoff                                       45
171100160909     C                   if        depSPE  = *zeros OR
171200160909     C                             depSPE <> vabNSP
171300160909     C                   seton                                        45
171400160909     C                   eval      depSPE  = vabNSP
171500160909     C                   endif
171600160909     C*
171700160909     C                   if        not *in31
171800150420     C*
171900160902     C* Se primo giro allora scrivo testata
172000160902     C                   if        wVGD_Head = *zeros
172100160902     C                   eval      wVGD_Head = 1
172200160902     C                   eval      wBufferOut =
172300170316     C                             %trim(pptCHR)+'VABATB' +%trim(pptCHR)+pptCSV+
172400160902     C                             %trim(pptCHR)+'VABCCM' +%trim(pptCHR)+pptCSV+
172500160902     C                             %trim(pptCHR)+'VABLNP' +%trim(pptCHR)+pptCSV+
172600160902     C                             %trim(pptCHR)+'VABCTR' +%trim(pptCHR)+pptCSV+
172700160902     C                             %trim(pptCHR)+'VABRSD' +%trim(pptCHR)+pptCSV+
172800160902     C                             %trim(pptCHR)+'VABRD2' +%trim(pptCHR)+pptCSV+
172900160902     C                             %trim(pptCHR)+'VABIND' +%trim(pptCHR)+pptCSV+
173000160902     C                             %trim(pptCHR)+'VABLOD' +%trim(pptCHR)+pptCSV+
173100160902     C                             %trim(pptCHR)+'VABCAD' +%trim(pptCHR)+pptCSV+
173200160902     C                             %trim(pptCHR)+'VABPRD' +%trim(pptCHR)+pptCSV+
173300160902     C                             %trim(pptCHR)+'VABCBO' +%trim(pptCHR)+pptCSV+
173400160902     C                             %trim(pptCHR)+'VABRMO' +%trim(pptCHR)+pptCSV+
173500160902     C                             %trim(pptCHR)+'VABRMN' +%trim(pptCHR)+pptCSV+
173600160902     C                             %trim(pptCHR)+'VABRMA' +%trim(pptCHR)+pptCSV+
173700160902     C                             %trim(pptCHR)+'VABNAS' +%trim(pptCHR)+pptCSV+
173800160902     C                             %trim(pptCHR)+'VABNCL' +%trim(pptCHR)+pptCSV+
173900160902     C                             %trim(pptCHR)+'VABPKB' +%trim(pptCHR)+pptCSV+
174000160902     C                             %trim(pptCHR)+'VABCAS' +%trim(pptCHR)+pptCSV+
174100160902     C                             %trim(pptCHR)+'VABVCA' +%trim(pptCHR)+pptCSV+
174200160902     C                             %trim(pptCHR)+'VABFFD' +%trim(pptCHR)+pptCSV+
174300160902     C                             %trim(pptCHR)+'VABIAS' +%trim(pptCHR)+pptCSV+
174400160902     C                             %trim(pptCHR)+'VABVAS' +%trim(pptCHR)+pptCSV+
174500160902     C                             %trim(pptCHR)+'VABNOT' +%trim(pptCHR)+pptCSV+
174600170510     C                             %trim(pptCHR)+'VABTSP' +%trim(pptCHR)+pptCSV+
174700160902     C                             %trim(pptCHR)+'VABNRC' +%trim(pptCHR)+pptCSV+
174800160902     C                             %trim(pptCHR)+'VABTRC' +%trim(pptCHR)+pptCSV+
174900160902     C                             %trim(pptCHR)+'VABEMD' +%trim(pptCHR)+pptCSV+
175000160902     C                             %trim(pptCHR)+'VABCEL' +%trim(pptCHR)
175100160902     C                   exsr      wriVGD
175200160902     C                   endif
175300160902     C*
175400160902     C* Quindi scrivo dati spedizione
175500160909     C   45              eval      wBufferOut =
175600170316     C                             %trim(pptCHR)+%trim(VABATB)
175700170316     C                                                    +%trim(pptCHR)+pptCSV+
175800150420     C                             %trim(pptCHR)+%trim(%editc(VABCCM:'X'))
175900150420     C                                                    +%trim(pptCHR)+pptCSV+
176000150420     C                             %trim(pptCHR)+%trim(%editc(VABLNP:'X'))
176100150420     C                                                    +%trim(pptCHR)+pptCSV+
176200150420     C                             %trim(pptCHR)+%trim(%editc(VABCTR:'X'))
176300150420     C                                                    +%trim(pptCHR)+pptCSV+
176400150420     C                             %trim(pptCHR)+%trim(VABRSD)
176500150420     C                                                    +%trim(pptCHR)+pptCSV+
176600160902     C                             %trim(pptCHR)+%trim(VABRD2)
176700160902     C                                                    +%trim(pptCHR)+pptCSV+
176800150420     C                             %trim(pptCHR)+%trim(VABIND)
176900150420     C                                                    +%trim(pptCHR)+pptCSV+
177000150420     C                             %trim(pptCHR)+%trim(VABLOD)
177100150420     C                                                    +%trim(pptCHR)+pptCSV+
177200150420     C                             %trim(pptCHR)+%trim(VABCAD)
177300150420     C                                                    +%trim(pptCHR)+pptCSV+
177400150420     C                             %trim(pptCHR)+%trim(VABPRD)
177500150420     C                                                    +%trim(pptCHR)+pptCSV+
177600150420     C                             %trim(pptCHR)+%trim(VABCBO)
177700150420     C                                                    +%trim(pptCHR)+pptCSV+
177800150420     C                             %trim(pptCHR)+%trim(VABRMO)
177900150420     C                                                    +%trim(pptCHR)+pptCSV+
178000150420     C                             %trim(pptCHR)+%trim(%editc(VABRMN:'4'))
178100150420     C                                                    +%trim(pptCHR)+pptCSV+
178200150420     C                             %trim(pptCHR)+%trim(VABRMA)
178300150420     C                                                    +%trim(pptCHR)+pptCSV+
178400150420     C                             %trim(pptCHR)+%trim(VABNAS)
178500150420     C                                                    +%trim(pptCHR)+pptCSV+
178600150420     C                             %trim(pptCHR)+%trim(%editc(VABNCL:'4'))
178700150420     C                                                    +%trim(pptCHR)+pptCSV+
178800150420     C                             %trim(pptCHR)+%trim(%editc(VABPKB:'4'))
178900150420     C                                                    +%trim(pptCHR)+pptCSV+
179000150420     C                             %trim(pptCHR)+%trim(%editc(VABCAS:'4'))
179100150420     C                                                    +%trim(pptCHR)+pptCSV+
179200150420     C                             %trim(pptCHR)+%trim(VABVCA)
179300150420     C                                                    +%trim(pptCHR)+pptCSV+
179400150420     C                             %trim(pptCHR)+%trim(VABFFD)
179500150420     C                                                    +%trim(pptCHR)+pptCSV+
179600150420     C                             %trim(pptCHR)+%trim(%editc(VABIAS:'4'))
179700150420     C                                                    +%trim(pptCHR)+pptCSV+
179800150420     C                             %trim(pptCHR)+%trim(VABVAS)
179900150420     C                                                    +%trim(pptCHR)+pptCSV+
180000150421     C                             %trim(pptCHR)+%trim(VABNOT)
180100150421     C                                                    +%trim(pptCHR)+pptCSV+
180200170510     C                             %trim(pptCHR)+%trim(VABTSP)
180300170510     C                                                    +%trim(pptCHR)+pptCSV+
180400160902     C                             %trim(pptCHR)+%trim(wVATNOT_A)
180500160902     C                                                    +%trim(pptCHR)+pptCSV+
180600160902     C                             %trim(pptCHR)+%trim(wVATNOT_B)
180700160902     C                                                    +%trim(pptCHR)+pptCSV+
180800150420     C                             %trim(pptCHR)+%trim(wVATNOT_I)
180900150420     C                                                    +%trim(pptCHR)+pptCSV+
181000150420     C                             %trim(pptCHR)+%trim(wVATNOT_S)
181100150420     C                                                    +%trim(pptCHR)
181200160909     C   45              exsr      wriVGD
181300150420     C*
181400160909     C                   endif
181500160909     C*
181600150420     C                   ENDSR
181700150420     C*------------------------------------------------------------------------*
181800150420
181900150420
182000150420
182100150420      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
182200150420     C     wriVGD        BEGSR
182300150420     C*
182400150420     C                   clear                   tivgd000
182500150420     C                   eval      vgdDTA = wBufferOut
182600150420     C                   eval      vgdTIP = 'EB'
182700150420     C                   eval      vgdKSU = vlrKSC
182800150420     C                   eval      vgdTSC = 'WW'
182900150420     C                   eval      vgdDAT = datcor
183000160906     C                   eval      vgdPRG = f_tis7prgf
183100160920     C                   eval      vgdPGM = procname
183200160906     C                   eval      vgdSTO = '?'
183300150420     C                   write     tivgd000
183400150420     C*
183500150420     C                   ENDSR
183600150420     C*------------------------------------------------------------------------*
183700160902
183800160902
183900160902     C*----------------------------------------------------*
184000160902     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
184100160902     C*----------------------------------------------------*
184200160902     C     SR_SPLIT      BEGSR
184300160902     C*
184400160902     C* Chiamo il *pgm d utilità x lo "split" della riga delle intestazioni
184500160902     C                   CALL      'XSPLIT2'
184600160902     C                   PARM                    InStringa
184700160902     C                   PARM                    InSepara
184800160902     C                   PARM      '5'           InTypeOut
184900160902     C                   PARM      ''            wSkParam
185000160902     C                   PARM                    OutErrore
185100160902     C                   PARM                    OutMsg
185200160902     C                   MOVEA     wSkParam      SkSplitCSV_5
185300160902     C*
185400160902     C                   ENDSR
185500170316
185600170316
185700170316
185800170316     C*--------------------------------------------------------
185900170316     C* CARTAB - CARICAMENTO DATI TABELLARI                   *
186000170316     C*--------------------------------------------------------
186100170316     C     CARTAB        BEGSR
186200170316     C*
186300170316     C* TABELLA '15' - NAZIONI
186400170316     C                   clear                   skNAZISO
186500170316     C                   clear                   skNAZBAR
186600170316     C                   eval      tblKUT = 1
186700170316     C                   eval      tblCOD = '15'
186800170316     C     KEYtabP       setll     tabel00f
186900170316     C     KEYtabP       reade     tabel00f
187000170316     C                   dow       not %eof(tabel00f)
187100170316     C                   if        tblFLG = *blanks
187200170316     C                   movel(p)  tblUNI        ds15
187300170316     C                   add       1             jNAZ
187400170316     C                   eval      skNAZBAR(jNAZ) = tblKEY
187500170316     C                   eval      skNAZISO(jNAZ) = §15ISO
187600170316     C                   endif
187700170316     C     KEYtabP       reade     tabel00f
187800170316     C                   enddo
187900170316     C*
188000170316     C                   ENDSR
188100170316     C***
188200170316
188300150420
188400150420
188500000613     C     *inzsr        BEGSR
188600990910     C*
188700990910     C     *entry        plist
188800990920     C                   parm                    tivlrds
188900990921     C                   parm      wrkesito      esito
189000000724     C                   parm                    prmlit
189100000710     C                   parm                    prmfir
189200010330     C*
189300010330     C* CALCOLA LA DATA CORRENTE
189400150420     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
189500150420     C                   eval      datcor = %dec(%date() : *ISO)
189600170316     C*
189700170316     C* Chiave su TABEL00F - parziale
189800170316     C     KEYtabP       KLIST
189900170316     C                   KFLD                    tblKUT
190000170316     C                   KFLD                    tblCOD
190100000613     C*
190200000613     C                   ENDSR
190300000613     C***
