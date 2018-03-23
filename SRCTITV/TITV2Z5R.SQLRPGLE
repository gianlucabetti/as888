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
003400170322     D wVABRSD_1       s             35    inz
003500170322     D wVABRSD_2       s             35    inz
003600160902     D wVABRMA_1       s             15    inz
003700160902     D wVABRMA_2       s             15    inz
003800160902     D wVABNOT         s             35    inz
003900160906     D wVABCTR_E       s                   like(VABCTR) inz
004000150420     D wrkesito        s                   like(esito)
004100160902     D wVGD_Head       s              1  0 inz(*zeros)
004200160902     D wFlgCAS         s              1  0 inz(*zeros)
004300160902     D s_vindta        s                   like(vindta) inz(*blanks)
004400160908     D parccm          s              8    inz(*blanks)
004500160908     D parmbr          s             10    inz(*blanks)
004600160908     D paropz          s              1    inz(*blanks)
004700160908     D chkcall         s              1    inz(*blanks)
004800160909     D depSPE          s              7  0 inz
004900170316
005000170316     D*------------
005100170316     D jNAZ            s              5  0 INZ(*zeros)
005200170316     D skNAZISO        S              3    DIM(1000)
005300170316     D skNAZBAR        S              3    DIM(1000)
005400160902
005500160908
005600160908     D*------------------
005700160908     D* DS REPERIMENTO NUMERATORE
005800160908     D*------------------
005900160908     D trul33ds      e ds                  inz
006000160908     D*------------------
006100160908     D* DS ARCHITETTURA
006200160908     D*------------------
006300160908     D KPJBA         e ds                  inz
006400160908
006500160902
006600160902     D*------------------
006700160902     D* VARIABILI X LA CHIAMATA AL *PGM UTILITA' "XSPLIT"
006800160902     D*------------------
006900160902     D InStringa       S          65535A   VARYING                              (stringa input)
007000160902     D InSepara        S             10A                                        (separatore)
007100160902     D InTypeOut       S              1A                                        (tipo output)
007200160902     D wSkParam        S          65535A                                        (output)
007300160902     D OutErrore       S              1A                                        (flag errore)
007400160902     D OutMsg          S             80A                                        (messaggio errore)
007500160902     D SkSplitCSV_5    S            256    DIM(256)
007600160902
007700160902
007800030715     D*------------------
007900030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
008000030715     D*------------------
008100160902     D SkSplitFLD      S                   LIKE(SkSplitCSV_5) DIM(256)
008200160902     D SkSplitCSV      S                   LIKE(SkSplitCSV_5) DIM(256)
008300160902     D CharCSV         S              1
008400160902     D CharTXT         S              1
008500160902     D CharNUM         S              1
008600160902     D CharSOS         S              1
008700160902     D i               s              3  0 INZ(1)
008800030715     D wGiro           s              1  0 INZ(*zeros)
008900030822     D*------------------
009000030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
009100030822     D*------------------
009200160902     D posDaDft        S              3  0 INZ(*zeros)
009300160902     D posADft         S              3  0 INZ(*zeros)
009400160902     D wGiroDft        s              1  0 INZ(*zeros)
009500160908
009600160908
009700031201     D*------------------
009800031201     D* Costanti
009900031201     D*------------------
010000160902     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
010100160908     D MAIU            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
010200150420
010300010330
010400010330
010500990921     C                   reset                   esito
010600990921     C                   reset                   wrkesito
010700010601     C*
010800170316     C                   exsr      cartab
010900160908     C                   exsr      defcam
011000160908     C   50              exsr      calprog
011100150420     C                   exsr      rwfile
011200160908     C   50              exsr      strvgd
011300160906     C                   exsr      endela
011400160906     C*
011500010601     C                   seton                                        lr
011600160906
011700160906
011800160906     C*--------------------------------------------------------
011900160906     C* STRVGD - "LIBERO" LA TRANSAZIONE (GRUPPO) CORRENTE    *
012000160906     C*--------------------------------------------------------
012100160906     C     STRVGD        BEGSR
012200160906     C*
012300160906     C
012400170119     C/EXEC SQL
012500170227     C+ UPDATE TIVGD00F SET VGDSTO = 'R'
012600170119     C+ WHERE VGDSTO = '?'     AND VGDPRG = :VGDPRG AND
012700170119     C+       VGDTIP = :VGDTIP AND VGDKSU = :VGDKSU AND
012800170119     C+       VGDTSC = :VGDTSC
012900170119     C/END-EXEC
013000160906     C
013100160906     C*
013200160906     C                   ENDSR
013300160906     C***
013400160902
013500010601
013600010601     C*--------------------------------------------------------
013700160902     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
013800010601     C*--------------------------------------------------------
013900010601     C     ENDELA        BEGSR
014000080222     C*
014100080222     C* Effettuo lancio TISI95 solo x chiusura
014200080222     C                   CLEAR                   TISI95DS
014300080222     C                   EVAL      I95TLA = 'C'
014400080222     C                   CALL      'TISI95R'
014500080222     C                   PARM                    TISI95DS
014600160908     C*
014700160908     C* Effettuo la chiamata al CLLE preposto
014800160908     C  N50              call(e)   'TITVVTC'
014900160908     C                   parm                    parccm
015000160908     C                   parm                    parmbr
015100160908     C                   parm      '2'           paropz
015200000613     C*
015300010601     C                   ENDSR
015400010601     C***
015500010601
015600010601
015700010330     C*--------------------------------------------------------
015800150420     C* RWFILE  LEGGE TIVIN00R E SCRIVE FIVABWWF              *
015900010330     C*--------------------------------------------------------
016000150420     C     RWFILE        BEGSR
016100010330     C*
016200010330     C                   if        not %open(tivin00r)
016300010330     C                   open      tivin00r
016400010330     C                   endif
016500160908     C*
016600160908     C* Eseguo operazioni di aggiunta nuovo membro file FNVAT
016700160908     C  N50              exsr      prevat
016800160908     C   50              eval      chkcall = '0'
016900160908     C*
017000160908     C                   if        chkcall = '0'
017100160908     C*
017200160908     C                   if        not %open(fivabwwr)
017300160908     C                   open      fivabwwr
017400160908     C                   endif
017500160908     C*
017600160908     C                   if        not %open(fivatwwr)
017700160908     C                   open      fivatwwr
017800160908     C                   endif
017900010330     C*
018000160908     C                   clear                   §CTROKVB          7 0
018100160908     C                   clear                   §CTROKVT          7 0
018200160908     C                   clear                   §CTRMO            7 0
018300160908     C                   clear                   §CTRNO            7 0
018400010330     C*
018500030822     C                   do        *HIVAL
018600010330     C*
018700030822     C                   read      tivin00r                               70
018800160902     C*
018900160902     C* Salvo il buffer orginale ricevuto dal cliente
019000160902     C                   eval      s_vindta = vindta
019100160902     C*
019200160902     C                   if        *in70 = *off and
019300160902     C                             vindta <> *blanks and
019400160902     C                             (vinflg = *blanks
019500160902     C                              or vinflg = '0'
019600160902     C                              or vinflg = '2')
019700160902     C*
019800160902     C* Controlli di "base" sui dati grezzi
019900160902     C                   exsr      normCSV
020000040510     C*
020100160902     C                   if        wFlgVINDTA = 1
020200010330     C*
020300010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
020400010711     C                   if        vinflg = *blanks or vinflg = '0'
020500010711     C                   clear                   vinmsg
020600010711     C                   endif
020700160902     C*
020800160902 xxx C* Porto eventualmente da minuscolo in maiuscolo le INTESTAZIONI colonne cliente
020900160908 xxx C***  minu:MAIU     XLATE     vindta        vindta
021000160902     C*
021100160902     C* Splitto i dati
021200160902     C                   CLEAR                   SkSplitCSV_5
021300160902     C                   EVAL      InStringa = vindta
021400160902     C                   EVAL      InSepara  = CharCSV
021500160902     C                   EXSR      SR_SPLIT
021600160902     C*
021700160902     C*****************************
021800160909 xxx C* A questo punto verifico che le colonne mandatorie siano valorizzate correttamente
021900170322     C                   if        %len(%trim(SkSplitCSV_5(1)))  > 0 and
022000170322     C                             %len(%trim(SkSplitCSV_5(30))) > 3 and
022100170322     C                             %len(%trim(SkSplitCSV_5(32))) > 3 and
022200170322     C                             %len(%trim(SkSplitCSV_5(35))) > 2
022300160902     C*****************************
022400010601     C*
022500160902     C* Se OK => procedo con elaborazione
022600010330     C                   exsr      inzvar
022700010605     C                   exsr      impvab
022800010601     C*
022900010604     C* Ebbene...
023000160908     C                   if        wGiro > 1
023100160908     C  N31              add       1             §CTROKVB
023200160908     C   32              add       1             §CTRMO
023300160908     C   31              add       1             §CTRNO
023400160908     C*
023500160902     C   50              exsr      wriVAB_VGD
023600160908     C  N50              exsr      wriVAB_WW
023700150420     C                   endif
023800010604     C*
023900010604     C                   if        *in31 = *off and
024000010604     C                             *in32 = *off
024100010604     C                   eval      vinflg = '1'
024200010604     C                   else
024300010604     C                   eval      vinflg = '2'
024400010604     C                   endif
024500010601     C*
024600160902     C                   else
024700160902     C                   eval      vinflg = '1'
024800010604     C                   endif
024900160902     C*
025000160902     C                   else
025100160902     C                   eval      vinflg = '1'
025200160902     C                   endif
025300010604     C*
025400010330     C                   else
025500010330     C                   eval      vinflg = '1'
025600010330     C                   endif
025700010601     C*
025800160902     C                   eval      vindta = s_vindta
025900010601     C  N70              update    tivin000
026000010330     C*
026100030822     C  N70              enddo
026200010601     C
026300010601     C*
026400160902     C* Stabilisco l'esito della traduzione
026500160902     C                   select
026600160902     C*
026700160902     C* Se non ci sono record con errori => stato OK
026800160902     C                   when      §ctrno = 0 and
026900160902     C                             §ctrmo = 0
027000010330     C                   eval      wrkesito = '0'
027100160902     C*
027200160902     C* Se nessun record OK => errore totale
027300160908     C                   when      §ctrokvb = 0
027400160902     C                   eval      wrkesito = '2'
027500160902     C*
027600160902     C* Diversamente => errore parziale/warning
027700160902     C                   other
027800160902     C                   eval      wrkesito = '1'
027900160902     C*
028000160902     C                   endsl
028100160908     C*
028200160908     C                   endif
028300160908     C
028400160908     C                   if        chkcall = '1'
028500160908     C                   eval      wrkesito = '2'
028600160908     C                   endif
028700010330     C*
028800010330     C                   if        %open(tivin00r)
028900010330     C                   close     tivin00r
029000010330     C                   endif
029100160908     C*
029200160908     C                   if        %open(fivabwwr)
029300160908     C                   close     fivabwwr
029400160908     C                   endif
029500160908     C                   if        %open(fivatwwr)
029600160908     C                   close     fivatwwr
029700160908     C                   endif
029800160908     C*
029900160908     C                   if        not *in50        and
030000160908     C                             wrkesito <> '2'  and
030100160908     C                             vlrpoi <> *zeros
030200160908     C                   exsr      invio
030300160908     C                   endif
030400010330     C*
030500010330     C                   ENDSR
030600010330     C***
030700160908
030800160908
030900160908     C*----------------------------------------------------*
031000160908     C*  SCARICAMENTO BUFFER RECORDS VAB IN "WW"
031100160908     C*----------------------------------------------------*
031200160908     C     WRIVAB_WW     BEGSR
031300160908     C*
031400160908     C* Verifica campi "particolari" VAB
031500160908     C                   exsr      repNSP
031600160909     C*
031700160909     C                   setoff                                       45
031800160909     C                   if        depSPE  = *zeros OR
031900160909     C                             depSPE <> vabNSP
032000160909     C                   seton                                        45
032100160909     C                   eval      depSPE  = vabNSP
032200160909     C                   endif
032300160909     C*
032400160909     C                   if        not *in31
032500160909     C                   exsr      EXEVATE
032600160909     C   45              exsr      EXEVATA
032700160909     C   45              exsr      EXEVATB
032800160909     C   45              exsr      EXEVATI
032900160909     C   45              exsr      EXEVATJ
033000160909     C   45              exsr      EXEVATS
033100160909     C   45              write     fivab000                                     => scarico il VAB
033200160909     C                   endif
033300160908     C*
033400160908     C                   ENDSR
033500160908
033600160908
033700160908     C*----------------------------------------------------*
033800160908     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
033900160908     C*----------------------------------------------------*
034000160908     C     REPNSP        BEGSR
034100160908     C*
034200160909     C* NSP => Se necessario stacco un numeratore da AZNUM
034300160909     C                   if        VABNSP = *zeros
034400160908     C                   clear                   TRUL33DS
034500160908     C                   eval      I33OPE = *zeros
034600160908     C                   eval      I33CNU = 302
034700160908     C                   eval      I33NUM = 1
034800160908     C                   movel     TRUL33DS      KPJBU
034900160908     C                   call      'TRUL33R'
035000160908     C                   parm                    KPJBA
035100160908     C                   movel     KPJBU         TRUL33DS
035200160908     C                   if        O33ERR = *zeros
035300160908     C                   z-add     O33NRF        VABNSP
035400160908     C                   z-add     O33NRF        VATNSP
035500160908     C                   else
035600160908     C                   SETON                                        31
035700160908     C                   EVAL      vinmsg = %trimr(vinmsg)
035800160908     C                             + ' ' + 'VABNSP VATNSP'
035900160908     C                   endif
036000160909     C                   endif
036100160908     C*
036200160908     C                   ENDSR
036300160908
036400160908
036500160908     C*----------------------------------------------------*
036600160908     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
036700160908     C*----------------------------------------------------*
036800160908     C     EXEVATA       BEGSR
036900160908     C*
037000160908     C                   EVAL      VATTRC = 'A'
037100160908     C                   EVAL      VATNOT = wVATNOT_A
037200160908     C                   exsr      wrivat                                       => scarico VAT
037300160908     C*
037400160908     C                   ENDSR
037500160908     C*----------------------------------------------------*
037600160908     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
037700160908     C*----------------------------------------------------*
037800160908     C     EXEVATB       BEGSR
037900160908     C*
038000160908     C                   EVAL      VATTRC = 'B'
038100160908     C                   EVAL      VATNOT = wVATNOT_B
038200160908     C                   exsr      wrivat                                       => scarico VAT
038300160908     C*
038400160908     C                   ENDSR
038500160909     C*----------------------------------------------------*
038600160909     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "I"
038700160909     C*----------------------------------------------------*
038800160909     C     EXEVATI       BEGSR
038900160909     C*
039000160909     C                   IF        %len(%trim(wVATNOT_I)) > 35
039100160909     C                   EVAL      VATTRC = 'I'
039200160909     C                   EVAL      VATNOT = %subst(%trim(wVATNOT_I):1:35)
039300160909     C                   exsr      wrivat                                       => scarico VAT
039400160909     C                   EVAL      VATTRC = 'J'
039500160909     C                   EVAL      VATNOT = %subst(%trim(wVATNOT_I):36)
039600160909     C                   exsr      wrivat                                       => scarico VAT
039700160909     C                   ELSE
039800160909     C                   EVAL      VATTRC = 'I'
039900160909     C                   EVAL      VATNOT = wVATNOT_I
040000160909     C                   exsr      wrivat                                       => scarico VAT
040100160909     C                   ENDIF
040200160909     C*
040300160909     C                   ENDSR
040400160909     C*----------------------------------------------------*
040500160909     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "J"
040600160909     C*----------------------------------------------------*
040700160909     C     EXEVATJ       BEGSR
040800160909     C*
040900160909     C                   IF        %len(%trim(wVATNOT_I)) <= 35
041000160909     C                   EVAL      VATTRC = 'J'
041100160909     C                   EVAL      VATNOT = wVATNOT_J
041200160909     C                   exsr      wrivat                                       => scarico VAT
041300160909     C                   ENDIF
041400160909     C*
041500160909     C                   ENDSR
041600160908     C*----------------------------------------------------*
041700160908     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "S"
041800160908     C*----------------------------------------------------*
041900160908     C     EXEVATS       BEGSR
042000160908     C*
042100160908     C                   EVAL      VATTRC = 'S'
042200160908     C                   EVAL      VATNOT = wVATNOT_S
042300160908     C                   exsr      wrivat                                       => scarico VAT
042400160908     C*
042500160908     C                   ENDSR
042600160908     C*----------------------------------------------------*
042700160908     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
042800160908     C*----------------------------------------------------*
042900160908     C     EXEVATE       BEGSR
043000160908     C*
043100160908     C                   EVAL      VATTRC = 'E'
043200160908     C                   EVAL      VATNOT = wVATNOT_E
043300160908     C                   exsr      wrivat                                       => scarico VAT
043400160908     C*
043500160908     C                   ENDSR
043600160908     C*----------------------------------------------------*
043700160908     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
043800160908     C*----------------------------------------------------*
043900160908     C     WRIVAT        BEGSR
044000160908     C*
044100160908     C* Scrivo solo se valorizzato qualcosa
044200160908     C                   IF        VATNOT <> *blanks
044300160908     C                   EVAL      VATFGS = VABFGS
044400160908     C                   EVAL      VATCCM = VABCCM
044500160908     C                   EVAL      VATLNP = VABLNP
044600160908     C                   EVAL      VATAAS = VABAAS
044700160908     C                   EVAL      VATNRS = VABNRS
044800160908     C                   EVAL      VATNSP = VABNSP
044900160908     C                   WRITE     FIVAT000
045000160908     C                   ADD       1             §CTROKVT
045100160908     C                   ENDIF
045200160908     C*
045300160908     C                   ENDSR
045400160908
045500160908
045600160908     C*----------------------------------------------------*
045700160908     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE ACCESSORIO
045800160908     C*----------------------------------------------------*
045900160908     C     PREVAT        BEGSR
046000160908     C*
046100160908     C* Compongo il nome del membro da dare al FIVATWWR
046200160908     C                   eval      parmbr = vlrhdl
046300160908     C                   movel     'M'           parmbr
046400160908     C                   eval      parccm = %subst(vlrKSC:2:7)
046500160908     C                   eval      paropz = '1'
046600160908     C*
046700160908     C* Effettuo la chiamata al CLLE preposto
046800160908     C                   call(e)   'TITVVTC'
046900160908     C                   parm                    parccm
047000160908     C                   parm                    parmbr
047100160908     C                   parm                    paropz
047200160908     C*
047300160908     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
047400160908     C                   if        %error
047500160908     C                   movel     '1'           chkcall
047600160908     C                   else
047700160908     C                   movel     '0'           chkcall
047800160908     C                   endif
047900160908     C*
048000160908     C                   ENDSR
048100160908
048200160908
048300160908      /TITLE Invio dei dati al punto operativo.
048400160908     C     invio         BEGSR
048500160908     C*
048600160908     C* 1° invio FIVAT
048700160908     C                   reset                   dscmz
048800160908     C                   move      vlrpoi        cmzdst
048900160908     C                   eval      cmzfld = 'FIVATWWR'
049000160908     C                   eval      cmzmbd = vlrhdl
049100160908     C                   eval      %subst(cmzmbd:1:1) = 'M'
049200160908     C***                if        prmfir = *blanks
049300160908     C                   eval      cmzfla = 'FIVAT00F'
049400160908     C                   eval      cmzmba = 'FIVAT00F'
049500160908     C***                else
049600160908     C***                eval      cmzfla = prmfir
049700160908     C***                eval      cmzmba = prmfir
049800160908     C***                endif
049900160908     C                   eval      cmznrr = *zeros
050000160908     C                   move      §ctrokvt      cmznrr
050100160908     C                   eval      cmzlba = vlrfl1
050200160908     C                   call(e)   'TIS711C'
050300160908     C                   parm                    dscmz
050400160908     C                   parm      *blanks       esito
050500160908     C                   if        %error
050600160908     C                             or cmzerr = '1'
050700160908     C                             or esito  = '1'
050800160908     C                   eval      wrkesito = '3'
050900160908     C                   else
051000160908     C*
051100160908     C* 2° invio FIVAB
051200160908     C                   reset                   dscmz
051300160908     C                   move      vlrpoi        cmzdst
051400160908     C                   eval      cmzfld = vlrfou
051500160908     C                   eval      cmzmbd = vlrhdl
051600160908     C                   eval      %subst(cmzmbd:1:1) = 'M'
051700160908     C***                if        prmfir = *blanks
051800160908     C                   eval      cmzfla = 'FIVAB00F'
051900160908     C                   eval      cmzmba = 'FIVAB00F'
052000160908     C***                else
052100160908     C***                eval      cmzfla = prmfir
052200160908     C***                eval      cmzmba = prmfir
052300160908     C***                endif
052400160908     C                   eval      cmznrr = *zeros
052500160908     C                   move      §ctrokvb      cmznrr
052600160908     C                   eval      cmzlba = vlrfl1
052700160908     C                   call(e)   'TIS711C'
052800160908     C                   parm                    dscmz
052900160908     C                   parm      *blanks       esito
053000160908     C                   if        %error
053100160908     C                             or cmzerr = '1'
053200160908     C                             or esito  = '1'
053300160908     C                   eval      wrkesito = '3'
053400160908     C                   endif
053500160908     C                   endif
053600160908     C*
053700160908     C                   ENDSR
053800160908     C***
053900010601
054000010601
054100010330     C*----------------------------------------------------*
054200030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
054300010330     C*----------------------------------------------------*
054400010330     C     INZVAR        BEGSR
054500010330     C*
054600030822     C* Inizializzo variabili di wrk
054700160902     C                   Z-ADD     *zeros        Num5_0            5 0
054800030822     C*
054900030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
055000160908     C                   CLEAR                   FIVAB000
055100160908     C                   CLEAR                   FIVAT000
055200170322     C                   CLEAR                   wVABRMA_1
055300170322     C                   CLEAR                   wVABRMA_2
055400170322     C                   CLEAR                   wVABRSD_1
055500170322     C                   CLEAR                   wVABRSD_2
055600160908     C                   CLEAR                   wVABNOT
055700160902     C                   CLEAR                   wVATNOT_A
055800160902     C                   CLEAR                   wVATNOT_B
055900160902     C                   CLEAR                   wVATNOT_E
056000150420     C                   CLEAR                   wVATNOT_I
056100160908     C                   CLEAR                   wVATNOT_J
056200150420     C                   CLEAR                   wVATNOT_S
056300030822     C                   CLEAR                   SkSplitCSV
056400160902     C                   CLEAR                   wFlgCAS
056500030822     C                   EVAL      i = 1
056600030822     C*
056700030822     C* Reimposto i valori di default
056800030822     C                   EXSR      DEFCAM
056900030822     C*
057000010330     C                   ENDSR
057100160902
057200160902
057300010330     C*----------------------------------------------------*
057400030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
057500010330     C*----------------------------------------------------*
057600010330     C     DEFCAM        BEGSR
057700110706     C*
057800160906     C                   SETOFF                                       7172
057900030822     C*
058000030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
058100030715     C* e delimitatore testo.
058200030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
058300170315     C*
058400170315     C* Verifico eventuali valori particolari per il carattere separatore campo
058500170315     C                   IF        CharCSV = '5'
058600170315     C                   EVAL      CharCSV = x'05'
058700170315     C                   ENDIF
058800170315     C*
058900030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
059000030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
059100150420     C                   IF        %subst(vlrppt:5:1) <> *blanks
059200150420     C                   EVAL      pptCSV = %subst(vlrppt:5:1)
059300150420     C                   ELSE
059400150420     C                   EVAL      pptCSV = ';'
059500150420     C                   ENDIF
059600150420     C                   IF        %subst(vlrppt:6:1) <> *blanks
059700150420     C                   EVAL      pptCHR = %subst(vlrppt:6:1)
059800150420     C                   ELSE
059900150420     C                   EVAL      pptCHR = ' '
060000150420     C                   ENDIF
060100090403     C*
060200090403     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
060300160902     C                   EVAL      CharSOS = CharNUM
060400030822     C*
060500030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
060600030715     C                   IF        %subst(vlrppt:1:1) = 'S'
060700160902     C                   SETON                                        50        * OUT => Download
060800030715     C                   ELSE
060900160902     C                   SETOFF                                       50        * OUT => File WW
061000030715     C                   ENDIF
061100030822     C*
061200030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
061300030822     C                   EVAL      posDaDft = 1
061400030822     C                   EVAL      posADft  = 0
061500030822     C                   EVAL      wGiroDft = 0
061600030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
061700030822     C                             posDaDft > 0
061800030822     C*
061900030822     C* Gestisco il 1° giro
062000030822     C                   IF        wGiroDft = 0
062100030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
062200030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
062300030822     C* Incremento il contatore dei "giri"
062400030822     C                   EVAL      wGiroDft = 1
062500030822     C                   ELSE
062600030822     C                   EVAL      posDaDft = posADft
062700030822     C                   ENDIF
062800030822     C* Eseguo lo scan x trovare la fine del campo corrente
062900030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
063000030822     C*
063100030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
063200030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
063300030822     C                   IF        posDaDft > 0 AND
063400030822     C                             posADft  > 0
063500110706     C* RMN
063600110706     C                   IF        %subst(
063700110706     C                             %subst(vlrppt:posDaDft+1:
063800110706     C                             posADft-posDaDft-1):1:3)
063900110706     C                             = 'RMN'
064000110706     C                   IF        %trim(%subst(
064100110706     C                             %subst(vlrppt:posDaDft+1:
064200110706     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064300110706     C                             = '*NSP'
064400160902     C                   SETON                                        71
064500110706     C                   ENDIF
064600110706     C                   ENDIF
064700060331     C* FGS
064800060331     C                   IF        %subst(
064900060331     C                             %subst(vlrppt:posDaDft+1:
065000060331     C                             posADft-posDaDft-1):1:3)
065100060331     C                             = 'FGS'
065200060331     C                   EVAL      PiStr=%trim(%subst(
065300060331     C                             %subst(vlrppt:posDaDft+1:
065400060331     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065500060331     C                   EXSR      CHKNUM
065600060331     C                   IF        PiInt=*on
065700060331     C                   Z-ADD     PiVal         VABFGS
065800060331     C                   ENDIF
065900060331     C                   ENDIF
066000030822     C* NCL
066100030822     C                   IF        %subst(
066200030822     C                             %subst(vlrppt:posDaDft+1:
066300030822     C                             posADft-posDaDft-1):1:3)
066400030822     C                             = 'NCL'
066500030822     C                   EVAL      PiStr=%trim(%subst(
066600030822     C                             %subst(vlrppt:posDaDft+1:
066700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066800030822     C                   EXSR      CHKNUM
066900030822     C                   IF        PiInt=*on
067000030822     C                   Z-ADD     PiVal         VABNCL
067100030822     C                   ENDIF
067200030822     C                   ENDIF
067300030822     C* CCM
067400030822     C                   IF        %subst(
067500030822     C                             %subst(vlrppt:posDaDft+1:
067600030822     C                             posADft-posDaDft-1):1:3)
067700030822     C                             = 'CCM'
067800030822     C                   EVAL      PiStr=%trim(%subst(
067900030822     C                             %subst(vlrppt:posDaDft+1:
068000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068100030822     C                   EXSR      CHKNUM
068200030822     C                   IF        PiInt=*on
068300030822     C                   Z-ADD     PiVal         VABCCM
068400030822     C                   ENDIF
068500030822     C                   ENDIF
068600030822     C* LNP
068700030822     C                   IF        %subst(
068800030822     C                             %subst(vlrppt:posDaDft+1:
068900030822     C                             posADft-posDaDft-1):1:3)
069000030822     C                             = 'LNP'
069100030822     C                   EVAL      PiStr=%trim(%subst(
069200030822     C                             %subst(vlrppt:posDaDft+1:
069300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069400030822     C                   EXSR      CHKNUM
069500030822     C                   IF        PiInt=*on
069600030822     C                   Z-ADD     PiVal         VABLNP
069700030822     C                   ENDIF
069800030822     C                   ENDIF
069900030822     C* NRS
070000030822     C                   IF        %subst(
070100030822     C                             %subst(vlrppt:posDaDft+1:
070200030822     C                             posADft-posDaDft-1):1:3)
070300030822     C                             = 'NRS'
070400030822     C                   EVAL      PiStr=%trim(%subst(
070500030822     C                             %subst(vlrppt:posDaDft+1:
070600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070700030822     C                   EXSR      CHKNUM
070800030822     C                   IF        PiInt=*on
070900030822     C                   Z-ADD     PiVal         VABNRS
071000030822     C                   ENDIF
071100030822     C                   ENDIF
071200160906     C* CTR
071300030822     C                   IF        %subst(
071400030822     C                             %subst(vlrppt:posDaDft+1:
071500030822     C                             posADft-posDaDft-1):1:3)
071600030822     C                             = 'CTR'
071700030822     C                   EVAL      PiStr=%trim(%subst(
071800030822     C                             %subst(vlrppt:posDaDft+1:
071900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072000030822     C                   EXSR      CHKNUM
072100030822     C                   IF        PiInt=*on
072200160906     C                   Z-ADD     PiVal         VABCTR
072300030822     C                   ENDIF
072400030822     C                   ENDIF
072500160906     C* CTE (ovvero CTR per Estero)
072600160906     C                   IF        %subst(
072700160906     C                             %subst(vlrppt:posDaDft+1:
072800160906     C                             posADft-posDaDft-1):1:3)
072900160906     C                             = 'CTE'
073000160906     C                   EVAL      PiStr=%trim(%subst(
073100160906     C                             %subst(vlrppt:posDaDft+1:
073200160906     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073300160906     C                   EXSR      CHKNUM
073400160906     C                   IF        PiInt=*on
073500160906     C                   Z-ADD     PiVal         wVABCTR_E
073600160906     C                   SETON                                        72
073700160906     C                   ENDIF
073800160906     C                   ENDIF
073900030822     C* PKB
074000030822     C                   IF        %subst(
074100030822     C                             %subst(vlrppt:posDaDft+1:
074200030822     C                             posADft-posDaDft-1):1:3)
074300030822     C                             = 'PKB'
074400030822     C                   EVAL      PiStr=%trim(%subst(
074500030822     C                             %subst(vlrppt:posDaDft+1:
074600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074700030822     C                   EXSR      CHKNUM
074800030822     C                   IF        PiNum=*on
074900030822     C                   Z-ADD     PiVal         VABPKB
075000030822     C                   ENDIF
075100030822     C                   ENDIF
075200030822     C* VLB
075300030822     C                   IF        %subst(
075400030822     C                             %subst(vlrppt:posDaDft+1:
075500030822     C                             posADft-posDaDft-1):1:3)
075600030822     C                             = 'VLB'
075700030822     C                   EVAL      PiStr=%trim(%subst(
075800030822     C                             %subst(vlrppt:posDaDft+1:
075900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076000030822     C                   EXSR      CHKNUM
076100030822     C                   IF        PiNum=*on
076200030822     C                   Z-ADD     PiVal         VABVLB
076300030822     C                   ENDIF
076400030822     C                   ENDIF
076500030822     C* QFT
076600030822     C                   IF        %subst(
076700030822     C                             %subst(vlrppt:posDaDft+1:
076800030822     C                             posADft-posDaDft-1):1:3)
076900030822     C                             = 'QFT'
077000030822     C                   EVAL      PiStr=%trim(%subst(
077100030822     C                             %subst(vlrppt:posDaDft+1:
077200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077300030822     C                   EXSR      CHKNUM
077400030822     C                   IF        PiNum=*on
077500030822     C                   Z-ADD     PiVal         VABQFT
077600030822     C                   ENDIF
077700030822     C                   ENDIF
077800170316     C* ATB
077900170316     C                   IF        %subst(
078000170316     C                             %subst(vlrppt:posDaDft+1:
078100170316     C                             posADft-posDaDft-1):1:3)
078200170316     C                             = 'ATB'
078300170316     C                   EVAL      VABATB=%trim(%subst(
078400170316     C                             %subst(vlrppt:posDaDft+1:
078500170316     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078600170316     C                   ENDIF
078700030822     C* CBO
078800030822     C                   IF        %subst(
078900030822     C                             %subst(vlrppt:posDaDft+1:
079000030822     C                             posADft-posDaDft-1):1:3)
079100030822     C                             = 'CBO'
079200030822     C                   EVAL      VABCBO=%trim(%subst(
079300030822     C                             %subst(vlrppt:posDaDft+1:
079400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079500030822     C                   ENDIF
079600030822     C* TSP
079700030822     C                   IF        %subst(
079800030822     C                             %subst(vlrppt:posDaDft+1:
079900030822     C                             posADft-posDaDft-1):1:3)
080000030822     C                             = 'TSP'
080100030822     C                   EVAL      VABTSP=%trim(%subst(
080200030822     C                             %subst(vlrppt:posDaDft+1:
080300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080400030822     C                   ENDIF
080500030822     C* VAS
080600030822     C                   IF        %subst(
080700030822     C                             %subst(vlrppt:posDaDft+1:
080800030822     C                             posADft-posDaDft-1):1:3)
080900030822     C                             = 'VAS'
081000030822     C                   EVAL      VABVAS=%trim(%subst(
081100030822     C                             %subst(vlrppt:posDaDft+1:
081200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081300030822     C                   ENDIF
081400030822     C* VCA
081500030822     C                   IF        %subst(
081600030822     C                             %subst(vlrppt:posDaDft+1:
081700030822     C                             posADft-posDaDft-1):1:3)
081800030822     C                             = 'VCA'
081900030822     C                   EVAL      VABVCA=%trim(%subst(
082000030822     C                             %subst(vlrppt:posDaDft+1:
082100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082200030822     C                   ENDIF
082300030822     C* GCA
082400030822     C                   IF        %subst(
082500030822     C                             %subst(vlrppt:posDaDft+1:
082600030822     C                             posADft-posDaDft-1):1:3)
082700030822     C                             = 'GCA'
082800030822     C                   EVAL      VABGCA=%trim(%subst(
082900030822     C                             %subst(vlrppt:posDaDft+1:
083000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
083100030822     C                   ENDIF
083200030822     C* CTM
083300030822     C                   IF        %subst(
083400030822     C                             %subst(vlrppt:posDaDft+1:
083500030822     C                             posADft-posDaDft-1):1:3)
083600030822     C                             = 'CTM'
083700030822     C                   EVAL      VABCTM=%trim(%subst(
083800030822     C                             %subst(vlrppt:posDaDft+1:
083900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
084000030822     C                   ENDIF
084100030822     C* FFD
084200030822     C                   IF        %subst(
084300030822     C                             %subst(vlrppt:posDaDft+1:
084400030822     C                             posADft-posDaDft-1):1:3)
084500030822     C                             = 'FFD'
084600030822     C                   EVAL      VABFFD=%trim(%subst(
084700030822     C                             %subst(vlrppt:posDaDft+1:
084800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
084900030822     C                   ENDIF
085000030822     C* VAD
085100030822     C                   IF        %subst(
085200030822     C                             %subst(vlrppt:posDaDft+1:
085300030822     C                             posADft-posDaDft-1):1:3)
085400030822     C                             = 'VAD'
085500030822     C                   EVAL      VABVAD=%trim(%subst(
085600030822     C                             %subst(vlrppt:posDaDft+1:
085700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
085800030822     C                   ENDIF
085900030822     C* GMA
086000030822     C                   IF        %subst(
086100030822     C                             %subst(vlrppt:posDaDft+1:
086200030822     C                             posADft-posDaDft-1):1:3)
086300030822     C                             = 'GMA'
086400030822     C                   EVAL      VABGMA=%trim(%subst(
086500030822     C                             %subst(vlrppt:posDaDft+1:
086600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
086700030822     C                   ENDIF
086800030822     C* GGA
086900030822     C                   IF        %subst(
087000030822     C                             %subst(vlrppt:posDaDft+1:
087100030822     C                             posADft-posDaDft-1):1:3)
087200030822     C                             = 'GGA'
087300030822     C                   EVAL      VABGGA=%trim(%subst(
087400030822     C                             %subst(vlrppt:posDaDft+1:
087500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
087600030822     C                   ENDIF
087700030822     C* GVA
087800030822     C                   IF        %subst(
087900030822     C                             %subst(vlrppt:posDaDft+1:
088000030822     C                             posADft-posDaDft-1):1:3)
088100030822     C                             = 'GVA'
088200030822     C                   EVAL      VABGVA=%trim(%subst(
088300030822     C                             %subst(vlrppt:posDaDft+1:
088400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
088500030822     C                   ENDIF
088600030822     C* TC1
088700030822     C                   IF        %subst(
088800030822     C                             %subst(vlrppt:posDaDft+1:
088900030822     C                             posADft-posDaDft-1):1:3)
089000030822     C                             = 'TC1'
089100030822     C                   EVAL      VABTC1=%trim(%subst(
089200030822     C                             %subst(vlrppt:posDaDft+1:
089300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
089400030822     C                   ENDIF
089500030822     C* TC2
089600030822     C                   IF        %subst(
089700030822     C                             %subst(vlrppt:posDaDft+1:
089800030822     C                             posADft-posDaDft-1):1:3)
089900030822     C                             = 'TC2'
090000030822     C                   EVAL      VABTC2=%trim(%subst(
090100030822     C                             %subst(vlrppt:posDaDft+1:
090200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
090300030822     C                   ENDIF
090400030822     C* ...
090500030822     C                   ENDIF
090600030822     C                   ENDDO
090700020204     C*
090800010330     C                   ENDSR
090900010607     C*----------------------------------------------------*
091000021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
091100010607     C*----------------------------------------------------*
091200010607     C     IMPVAB        BEGSR
091300010607     C*
091400010607     C                   SETOFF                                       3132
091500030515     C*
091600010607     C                   MOVEL     datcor        VABAAS
091700010607     C                   MOVE      datcor        VABMGS
091800160902     C*
091900160902     C* Dopo aver splittato nei vari campi eseguo 1 prima "normalizzazione" x eliminare
092000160902     C* i caratteri di delimitazione testo e gli apici
092100160902     C                   EVAL      SkSplitCSV = SkSplitCSV_5
092200160902     C                   EVAL      i = 1
092300160902     C                   DOW       i < %elem(SkSplitCSV)
092400160902     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
092500160902     C     '''':' '      XLATE     SkSplitCSV(i) SkSplitCSV(i)
092600160902     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
092700160902     C                   EVAL      i = i + 1
092800160902     C                   ENDDO
092900010607     C*
093000030715     C**********
093100160902     C* Valorizzo la lista delle INTESTAZIONI colonne cliente
093200030715     C**********
093300030715     C                   IF        wGiro = *zeros
093400160902     C*
093500160902     C                   EVAL      wGiro = 1
093600160902     C                   EVAL      SkSplitFLD = SkSplitCSV
093700160902     C*
093800160902     C                   ELSE
093900160902     C*
094000160902     C**********
094100160902     C* Elaboro le righe di DATI cliente
094200160902     C**********
094300160902     C                   EVAL      wGiro = 2
094400030715     C*
094500030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
094600030715     C                   EVAL      i = 1
094700030715     C                   DOW       i < %elem(SkSplitFLD)
094800030715     C*
094900030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
095000030715     C                   IF        SkSplitFLD(i) <> *blanks AND
095100030715     C                             SkSplitCSV(i) <> *blanks
095200130624     C*** ATB
095300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
095400150420     C                   EVAL      VABATB = SkSplitCSV(i)
095500030721     C                   ENDIF
095600170322     C*** RSD - 1
095700160902     C                   IF        %trim(SkSplitFLD(i)) =
095800170322     C                             'shipping_first_name'
095900170322     C                   EVAL      wVABRSD_1 = SkSplitCSV(i)
096000030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
096100170322     C     '@':'A'       XLATE     VABRSD        VABRSD
096200030715     C* ==
096300030715     C                   ENDIF
096400170322     C*** RSD - 2
096500170322     C                   IF        %trim(SkSplitFLD(i)) =
096600170322     C                             'shipping_last_name'
096700170322     C                   EVAL      wVABRSD_2 = SkSplitCSV(i)
096800170322     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
096900170322     C     '@':'A'       XLATE     VABRSD        VABRSD
097000170322     C* ==
097100170322     C                   ENDIF
097200030721     C*** RD2
097300160902     C                   IF        %trim(SkSplitFLD(i)) =
097400170322     C                             'shipping_address_2'
097500160902     C                   EVAL      VABRD2 = SkSplitCSV(i)
097600160902     C                   ENDIF
097700030715     C*** IND
097800160902     C                   IF        %trim(SkSplitFLD(i)) =
097900170322     C                             'shipping_address_1'
098000030715     C                   EVAL      VABIND = SkSplitCSV(i)
098100030715     C                   ENDIF
098200030715     C*** LOD
098300160902     C                   IF        %trim(SkSplitFLD(i)) =
098400170322     C                             'shipping_city'
098500030715     C                   EVAL      VABLOD = SkSplitCSV(i)
098600030715     C                   ENDIF
098700030715     C*** PRD
098800160902     C                   IF        %trim(SkSplitFLD(i)) =
098900170322     C                             'shipping_state'
099000030715     C                   EVAL      VABPRD = SkSplitCSV(i)
099100030715     C                   ENDIF
099200160902     C*** CAD
099300160902     C                   IF        %trim(SkSplitFLD(i)) =
099400170322     C                             'shipping_postcode'
099500160902     C                   EVAL      VABCAD = SkSplitCSV(i)
099600160902     C                   ENDIF
099700030721     C*** NZD
099800160902     C                   IF        %trim(SkSplitFLD(i)) =
099900170322     C                             'shipping_country'
100000170315     C                   EVAL      VABNZD = SkSplitCSV(i)
100100170316     C                   Z-ADD     1             jNAZ
100200170316     C     VABNZD        LOOKUP    skNAZISO(jNAZ)                         13
100300170316     C                   IF        %found
100400170316     C                   EVAL      VABNZD = skNAZBAR(jNAZ)
100500170316     C                   ENDIF
100600030721     C                   ENDIF
100700160902     C*** RMA - 1
100800170322     C                   IF        %trim(SkSplitFLD(i)) = 'order_id'
100900170315     C                   EVAL      wVABRMA_1 = SkSplitCSV(i)
101000150515     C                   ENDIF
101100160902     C*** RMA - 2
101200160908     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA_2'
101300160902     C                   EVAL      wVABRMA_2 = SkSplitCSV(i)
101400160902     C                   ENDIF
101500030715     C*** NOT
101600170322     C                   IF        %trim(SkSplitFLD(i)) = 'customer_note'
101700030715     C                   EVAL      VABNOT = SkSplitCSV(i)
101800030715     C                   ENDIF
101900030715     C*** NT2
102000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
102100030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
102200030715     C                   ENDIF
102300030715     C*** VCA
102400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
102500030715     C                   EVAL      VABVCA = SkSplitCSV(i)
102600030715     C                   ENDIF
102700030715     C*** VAS
102800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
102900030715     C                   EVAL      VABVAS = SkSplitCSV(i)
103000030715     C                   ENDIF
103100030715     C*** FFD
103200160902     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
103300030715     C                   EVAL      VABFFD = SkSplitCSV(i)
103400030715     C                   ENDIF
103500160902     C*** CBO
103600160902     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
103700160902     C                   EVAL      VABCBO = SkSplitCSV(i)
103800150420     C                   ENDIF
103900030721     C*** GC1
104000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
104100150420     C                   IF        %len(%trim(SkSplitCSV(i))) = 1
104200150420     C                   MOVEL     *blanks       wGC               1
104300150420     C                   EVAL      wGC = %trim(SkSplitCSV(i))
104400150420     C                   IF        SkSplitCSV(i) = 'M' OR
104500150420     C                             SkSplitCSV(i) = 'P'
104600150420     C                   MOVE(P)   wGC           VABGC1
104700150420     C                   ELSE
104800150420     C                   MOVEL(P)  wGC           VABGC1
104900150420     C                   ENDIF
105000150420     C                   ENDIF
105100030721     C                   ENDIF
105200030721     C*** GC2
105300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
105400120314     C                   IF        %len(%trim(SkSplitCSV(i))) = 1
105500120314     C                   MOVEL     *blanks       wGC               1
105600120314     C                   EVAL      wGC = %trim(SkSplitCSV(i))
105700120314     C                   IF        SkSplitCSV(i) = 'M' OR
105800120314     C                             SkSplitCSV(i) = 'P'
105900120314     C                   MOVE(P)   wGC           VABGC2
106000120314     C                   ELSE
106100120314     C                   MOVEL(P)  wGC           VABGC2
106200120314     C                   ENDIF
106300120314     C                   ENDIF
106400030721     C                   ENDIF
106500030721     C*** TSP
106600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
106700030721     C                   EVAL      VABTSP = SkSplitCSV(i)
106800030721     C                   ENDIF
106900030721     C*** NAS
107000160902     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
107100030721     C                   EVAL      VABNAS = SkSplitCSV(i)
107200030721     C                   ENDIF
107300030721     C*** TIC
107400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
107500030721     C                   EVAL      VABTIC = SkSplitCSV(i)
107600030721     C                   ENDIF
107700030721     C*** GCA
107800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
107900030721     C                   EVAL      VABGCA = SkSplitCSV(i)
108000030721     C                   ENDIF
108100030721     C*** XCO
108200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
108300030721     C                   EVAL      VABXCO = SkSplitCSV(i)
108400030721     C                   ENDIF
108500030721     C*** CTM
108600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
108700030721     C                   EVAL      VABCTM = SkSplitCSV(i)
108800030721     C                   ENDIF
108900030721     C*** TCR
109000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
109100030721     C                   EVAL      VABTCR = SkSplitCSV(i)
109200030721     C                   ENDIF
109300030721     C*** CTS
109400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
109500030721     C                   EVAL      VABCTS = SkSplitCSV(i)
109600030721     C                   ENDIF
109700030721     C*** FTM
109800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
109900030721     C                   EVAL      VABFTM = SkSplitCSV(i)
110000030721     C                   ENDIF
110100030721     C*** VAD
110200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
110300030721     C                   EVAL      VABVAD = SkSplitCSV(i)
110400030721     C                   ENDIF
110500030721     C*** GMA
110600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
110700030721     C                   EVAL      VABGMA = SkSplitCSV(i)
110800030721     C                   ENDIF
110900030721     C*** GGA
111000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
111100030721     C                   EVAL      VABGGA = SkSplitCSV(i)
111200030721     C                   ENDIF
111300030721     C*** GVA
111400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
111500030721     C                   EVAL      VABGVA = SkSplitCSV(i)
111600030721     C                   ENDIF
111700030721     C*** TC1
111800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
111900030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
112000030721     C                   ENDIF
112100030721     C*** TC2
112200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
112300030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
112400030721     C                   ENDIF
112500030721     C*** SCL
112600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
112700030721     C                   EVAL      VABSCL = SkSplitCSV(i)
112800030721     C                   ENDIF
112900030721     C*** RMO
113000160902     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
113100030721     C                   EVAL      VABRMO = SkSplitCSV(i)
113200030721     C                   ENDIF
113300160902     C*** CMO
113400160902     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
113500160902     C                   EVAL      VABCMO = SkSplitCSV(i)
113600160902     C                   ENDIF
113700030721     C*** NMO
113800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
113900030721     C                   EVAL      VABNMO = SkSplitCSV(i)
114000030721     C                   ENDIF
114100160908     C*** NOT_A
114200160908     C                   IF        %trim(SkSplitFLD(i)) =
114300170322     C                             'VATNOT_A'
114400160908     C                   EVAL      wVATNOT_A = SkSplitCSV(i)
114500160908     C                   ENDIF
114600160902     C*** NOT_B
114700160902     C                   IF        %trim(SkSplitFLD(i)) =
114800170322     C                             'billing_phone'
114900160902     C                   EVAL      wVATNOT_B = SkSplitCSV(i)
115000160902     C                   ENDIF
115100150420     C*** NOT_I
115200160902     C                   IF        %trim(SkSplitFLD(i)) =
115300170322     C                             'billing_email'
115400150420     C                   EVAL      wVATNOT_I = SkSplitCSV(i)
115500150420     C                   ENDIF
115600150420     C*** NOT_S
115700170315     C                   IF        %trim(SkSplitFLD(i)) =
115800170322     C                             'billing_phone'
115900150420     C                   EVAL      wVATNOT_S = SkSplitCSV(i)
116000150420     C                   ENDIF
116100030721     C*
116200070705     C* Reperisco quindi i campi numerici...
116300030721     C*** RMN
116400160908     C                   IF        %trim(SkSplitFLD(i)) =
116500170322     C                             'order_number'
116600030721     C                   EVAL      PiStr=SkSplitCSV(i)
116700030721     C                   EXSR      CHKNUM
116800030721     C                   IF        PiInt=*on
116900030721     C                   Z-ADD     PiVal         VABRMN
117000030721     C                   ELSE
117100030721     C                   SETON                                        32
117200030721     C                   EVAL      VABRMN = 1
117300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117400030721     C                             + ' ' + 'VABRMN'
117500030721     C                   ENDIF
117600030721     C                   ENDIF
117700030721     C*** NSP
117800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
117900030721     C                   EVAL      PiStr=SkSplitCSV(i)
118000030721     C                   EXSR      CHKNUM
118100100517     C                   IF        PiInt=*on
118200030721     C                   Z-ADD     PiVal         VABNSP
118300030721     C                   ELSE
118400030721     C                   SETON                                        32
118500030721     C                   EVAL      VABNSP = *zeros
118600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118700030721     C                             + ' ' + 'VABNSP'
118800030721     C                   ENDIF
118900030721     C                   ENDIF
119000160902     C*** NCL
119100160902     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
119200030721     C                   EVAL      PiStr=SkSplitCSV(i)
119300030721     C                   EXSR      CHKNUM
119400030721     C                   IF        PiInt=*on
119500030721     C                   Z-ADD     PiVal         VABNCL
119600030721     C                   ELSE
119700030721     C                   SETON                                        32
119800030721     C                   EVAL      VABNCL = *zeros
119900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120000030721     C                             + ' ' + 'VABNCL'
120100030721     C                   ENDIF
120200030721     C                   ENDIF
120300030721     C*** PKB
120400160902     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
120500030721     C                   EVAL      PiStr=SkSplitCSV(i)
120600030721     C                   EXSR      CHKNUM
120700030721     C                   IF        PiNum=*on
120800030721     C                   Z-ADD     PiVal         VABPKB
120900030721     C                   ELSE
121000030721     C                   SETON                                        32
121100030721     C                   EVAL      VABPKB = *zeros
121200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121300030721     C                             + ' ' + 'VABPKB'
121400030721     C                   ENDIF
121500030721     C                   ENDIF
121600160908     C*** CAS
121700160908     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
121800160908     C                   IF        SkSplitCSV(i) <> *blanks
121900160908     C                   EVAL      wFlgCAS = 1
122000160908     C                   ENDIF
122100160902     C                   EVAL      PiStr=SkSplitCSV(i)
122200160902     C                   EXSR      CHKNUM
122300160902     C                   IF        PiNum=*on
122400160902     C                   Z-ADD     PiVal         VABCAS
122500160902     C                   MOVEL     'EUR'         VABVCA
122600160902     C                   ELSE
122700160902     C                   SETON                                        32
122800160902     C                   EVAL      VABCAS = *zeros
122900160902     C                   EVAL      vinmsg = %trimr(vinmsg)
123000160902     C                             + ' ' + 'VABCAS'
123100160902     C                   ENDIF
123200160902     C                   ENDIF
123300030721     C*** CCM
123400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
123500030721     C                   EVAL      PiStr=SkSplitCSV(i)
123600030721     C                   EXSR      CHKNUM
123700030721     C                   IF        PiInt=*on      AND
123800030721     C                             PiVal<=9999999 AND
123900030721     C                             PiVal>*zeros
124000030721     C                   Z-ADD     PiVal         VABCCM
124100030721     C                   ELSE
124200030721     C                   SETON                                        32
124300030721     C                   EVAL      VABCCM = *zeros
124400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124500030721     C                             + ' ' + 'VABCCM'
124600030721     C                   ENDIF
124700030721     C                   ENDIF
124800030721     C*** LNP
124900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
125000030721     C                   EVAL      PiStr=SkSplitCSV(i)
125100030721     C                   EXSR      CHKNUM
125200030721     C                   IF        PiInt=*on    AND
125300030721     C                             PiVal<=999   AND
125400030721     C                             PiVal>*zeros
125500030721     C                   Z-ADD     PiVal         VABLNP
125600030721     C                   ELSE
125700030721     C                   SETON                                        32
125800030721     C                   EVAL      VABLNP = *zeros
125900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126000030721     C                             + ' ' + 'VABLNP'
126100030721     C                   ENDIF
126200030721     C                   ENDIF
126300030721     C*** NRS
126400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
126500030721     C                   EVAL      PiStr=SkSplitCSV(i)
126600030721     C                   EXSR      CHKNUM
126700030721     C                   IF        PiInt=*on AND
126800030721     C                             PiVal<=99
126900030721     C                   Z-ADD     PiVal         VABNRS
127000030721     C                   ELSE
127100030721     C                   SETON                                        32
127200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127300030721     C                             + ' ' + 'VABNRS'
127400030721     C                   ENDIF
127500030721     C                   ENDIF
127600030721     C*** LNA
127700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
127800120314     C                   IF        %trim(SkSplitCSV(i)) <> *zeros  AND
127900120314     C                             %trim(SkSplitCSV(i)) <> '0'     AND
128000120314     C                             %trim(SkSplitCSV(i)) <> '000'   AND
128100120314     C                             %trim(SkSplitCSV(i)) <> *blanks
128200030721     C                   EVAL      PiStr=SkSplitCSV(i)
128300030721     C                   EXSR      CHKNUM
128400030721     C                   IF        PiInt=*on    AND
128500030721     C                             PiVal<=999   AND
128600030721     C                             PiVal>*zeros
128700030721     C                   Z-ADD     PiVal         VABLNA
128800030721     C                   ELSE
128900030721     C                   SETON                                        32
129000030721     C                   EVAL      VABLNA = *zeros
129100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129200030721     C                             + ' ' + 'VABLNA'
129300120314     C                   ENDIF
129400030721     C                   ENDIF
129500030721     C                   ENDIF
129600030721     C*** CTR
129700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
129800030721     C                   EVAL      PiStr=SkSplitCSV(i)
129900030721     C                   EXSR      CHKNUM
130000030721     C                   IF        PiInt=*on  AND
130100030721     C                             PiVal<=999
130200030721     C                   Z-ADD     PiVal         VABCTR
130300030721     C                   ELSE
130400030721     C                   SETON                                        32
130500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130600030721     C                             + ' ' + 'VABCTR'
130700030721     C                   ENDIF
130800030721     C                   ENDIF
130900030721     C*** IAS
131000160902     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
131100030721     C                   EVAL      PiStr=SkSplitCSV(i)
131200030721     C                   EXSR      CHKNUM
131300030721     C                   IF        PiNum=*on
131400030721     C                   Z-ADD     PiVal         VABIAS
131500030721     C                   ELSE
131600030721     C                   SETON                                        32
131700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131800030721     C                             + ' ' + 'VABIAS'
131900030721     C                   ENDIF
132000030721     C                   ENDIF
132100030721     C*** VLB
132200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
132300030721     C                   EVAL      PiStr=SkSplitCSV(i)
132400030721     C                   EXSR      CHKNUM
132500030721     C                   IF        PiNum=*on
132600030721     C                   Z-ADD     PiVal         VABVLB
132700030721     C                   ELSE
132800030721     C                   SETON                                        32
132900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133000030721     C                             + ' ' + 'VABVLB'
133100030721     C                   ENDIF
133200030721     C                   ENDIF
133300030721     C*** QFT
133400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
133500030721     C                   EVAL      PiStr=SkSplitCSV(i)
133600030721     C                   EXSR      CHKNUM
133700030721     C                   IF        PiNum=*on
133800030721     C                   Z-ADD     PiVal         VABQFT
133900030721     C                   ELSE
134000030721     C                   SETON                                        32
134100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134200030721     C                             + ' ' + 'VABQFT'
134300030721     C                   ENDIF
134400030721     C                   ENDIF
134500030721     C*** NCD
134600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
134700030721     C                   EVAL      PiStr=SkSplitCSV(i)
134800030721     C                   EXSR      CHKNUM
134900030721     C                   IF        PiInt=*on      AND
135000030721     C                             PiVal<=9999999
135100030721     C                   Z-ADD     PiVal         VABNCD
135200030721     C                   ELSE
135300030721     C                   SETON                                        32
135400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135500030721     C                             + ' ' + 'VABNCD'
135600030721     C                   ENDIF
135700030721     C                   ENDIF
135800030721     C*** NCA
135900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
136000030721     C                   EVAL      PiStr=SkSplitCSV(i)
136100030721     C                   EXSR      CHKNUM
136200030721     C                   IF        PiInt=*on      AND
136300030721     C                             PiVal<=9999999
136400030721     C                   Z-ADD     PiVal         VABNCA
136500030721     C                   ELSE
136600030721     C                   SETON                                        32
136700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136800030721     C                             + ' ' + 'VABNCA'
136900030721     C                   ENDIF
137000030721     C                   ENDIF
137100030721     C*** ZNC
137200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
137300030721     C                   EVAL      PiStr=SkSplitCSV(i)
137400030721     C                   EXSR      CHKNUM
137500030721     C                   IF        PiInt=*on AND
137600030721     C                             PiVal<=99
137700030721     C                   Z-ADD     PiVal         VABZNC
137800030721     C                   ELSE
137900030721     C                   SETON                                        32
138000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138100030721     C                             + ' ' + 'VABZNC'
138200030721     C                   ENDIF
138300030721     C                   ENDIF
138400030721     C*** DCR
138500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
138600030721     C                   EVAL      PiStr=SkSplitCSV(i)
138700030721     C                   EXSR      CHKNUM
138800030721     C                   IF        PiInt=*on       AND
138900030721     C                             PiVal<=99999999
139000030721     C                   Z-ADD     PiVal         VABDCR
139100030721     C                   ELSE
139200030721     C                   SETON                                        32
139300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
139400030721     C                             + ' ' + 'VABDCR'
139500030721     C                   ENDIF
139600030721     C                   ENDIF
139700030721     C*** HCR
139800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
139900030721     C                   EVAL      PiStr=SkSplitCSV(i)
140000030721     C                   EXSR      CHKNUM
140100030721     C                   IF        PiInt=*on   AND
140200030721     C                             PiVal<=9999
140300030721     C                   Z-ADD     PiVal         VABHCR
140400030721     C                   ELSE
140500030721     C                   SETON                                        32
140600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
140700030721     C                             + ' ' + 'VABHCR'
140800030721     C                   ENDIF
140900030721     C                   ENDIF
141000030721     C*** VMD
141100160902     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
141200030721     C                   EVAL      PiStr=SkSplitCSV(i)
141300030721     C                   EXSR      CHKNUM
141400030721     C                   IF        PiNum=*on
141500030721     C                   Z-ADD     PiVal         VABVMD
141600030721     C                   ELSE
141700030721     C                   SETON                                        32
141800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
141900030721     C                             + ' ' + 'VABVMD'
142000030721     C                   ENDIF
142100030721     C                   ENDIF
142200030721     C*** ANT
142300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
142400030721     C                   EVAL      PiStr=SkSplitCSV(i)
142500030721     C                   EXSR      CHKNUM
142600030721     C                   IF        PiInt=*on        AND
142700030721     C                             PiVal<=999999999
142800030721     C                   Z-ADD     PiVal         VABANT
142900030721     C                   ELSE
143000030721     C                   SETON                                        32
143100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
143200030721     C                             + ' ' + 'VABANT'
143300030721     C                   ENDIF
143400030721     C                   ENDIF
143500030715     C*
143600030715     C                   ENDIF
143700030715     C                   EVAL      i = i + 1
143800030715     C                   ENDDO
143900070705     C*
144000160902     C* Considerazioni particolari "fuori ciclo"
144100110706     C*
144200160908     C* Verifica campi "particolari" VAB
144300160908     C                   EXSR      CHKVAB
144400160908     C*
144500030715     C                   ENDIF
144600020204     C*
144700010607     C                   ENDSR
144800010607     C*----------------------------------------------------*
144900160908
145000160908
145100160908     C*----------------------------------------------------*
145200160908     C*  CONSIDERAZIONI SU CAMPI PARTICOLARI VAB
145300160908     C*----------------------------------------------------*
145400160908     C     CHKVAB        BEGSR
145500160908     C*
145600160908     C*** RMN
145700160908     C                   IF        *in71
145800160908     C                   EVAL      VABRMN = VABNSP
145900160908     C                   ENDIF
146000160908     C                   IF        VABRMN = *zeros
146100160908     C                   EVAL      VABRMN = 1
146200160908     C                   ENDIF
146300160908     C*** NZD
146400160908     C                   IF        VABNZD = 'IT'  OR
146500160908     C                             VABNZD = 'ITA' OR
146600160908     C                             VABNZD = 'Ita' OR
146700160908     C                             VABNZD = 'ita'
146800160908     C                   EVAL      VABNZD = *blanks
146900160908     C                   ENDIF
147000160908     C*** CTR
147100160908     C                   IF        *in72
147200160908     C                   IF        VABNZD <> *blanks
147300160908     C                   EVAL      VABCTR =  wVABCTR_E
147400160908     C                   ENDIF
147500160908     C                   ENDIF
147600160908     C*** CBO
147700160908     C                   IF        VABCBO = *blanks
147800160908     C                   EVAL      VABCBO = '1'
147900160908     C                   ENDIF
148000160908     C*** NCL
148100160908     C                   IF        VABNCL = *zeros
148200160908     C                   EVAL      VABNCL = 1
148300160908     C                   ENDIF
148400160908     C*** CCM
148500160908     C                   IF        VABCCM = *zeros
148600160908     C                   EVAL      PiStr=VLRKSC
148700160908     C                   EXSR      CHKNUM
148800160908     C                   IF        PiInt=*on
148900160908     C                   Z-ADD     PiVal         VABCCM
149000160908     C                   ELSE
149100160908     C                   SETON                                        31
149200160908     C                   ENDIF
149300160908     C                   ENDIF
149400160908     C*** LNP
149500160908     C                   IF        VABLNP = *zeros
149600160908     C                   IF        VLRPOI = 046 OR
149700160908     C                             VLRPOI = 789 OR
149800160908     C                             VLRPOI = 999
149900160908     C                   MOVEL     VABCCM        VABLNP
150000160908     C                   ELSE
150100160908     C                   EVAL      VABLNP = VLRPOI
150200160908     C                   ENDIF
150300160908     C                   ENDIF
150400160908     C*** LNP
150500160908     C                   IF        VABFGS = *zeros
150600160908     C                   EVAL      VABFGS = VABLNP
150700160908     C                   ENDIF
150800160908     C*** FGS / LNP
150900160908     C                   IF        VABFGS = *zeros OR
151000160908     C                             VABLNP = *zeros
151100160908     C                   SETON                                        31
151200160908     C                   ENDIF
151300160908     C*** RMA
151400160908     C                   IF        wVABRMA_1 <> *blanks
151500160908     C                   EVAL      VABRMA = wVABRMA_1
151600160908     C                   ELSE
151700160920     C                   IF        wVABRMA_2 <> *blanks
151800160908     C                   EVAL      VABRMA = wVABRMA_2
151900160908     C                   ENDIF
152000160920     C                   ENDIF
152100160908     C*** CAS
152200160908     C                   IF        wFlgCAS = *zeros
152300160908     C                   EVAL      VABCAS = *zeros
152400160908     C                   EVAL      VABVCA = *blanks
152500160908     C                   ENDIF
152600160908     C*** NOT
152700160908     C                   IF        VABNOT = *blanks AND wVABNOT <> *blanks
152800160908     C                   EVAL      VABNOT = wVABNOT
152900160908     C                   ENDIF
153000170322     C*** RSD
153100170322     C                   EVAL      VABRSD = %trim(wVABRSD_1) + ' ' +
153200170322     C                                      %trim(wVABRSD_2)
153300160908     C*
153400160908     C* Se provincia nn valorizzata la reperisco
153500160908     C* tramite TISI95R a seconda dei dati d instradamento presenti
153600170316     C                   IF        VABNZD = *blanks AND
153700170316     C                             VABPRD = *blanks
153800160908     C                   CLEAR                   TISI95DS
153900160908     C                   EVAL      I95TCN = '3'
154000160908     C                   Z-ADD     datcor        I95DAT
154100160908     C                   EVAL      I95NAR = VABNZD
154200160908     C                   EVAL      I95CAP = VABCAD
154300160908     C                   EVAL      I95LOC = VABLOD
154400160908     C                   CALL      'TISI95R'
154500160908     C                   PARM                    TISI95DS
154600160908     C                   EVAL      VABPRD = O95PRV
154700160908     C                   ENDIF
154800160908     C*
154900160908     C* Eseguo routine finale x considerazioni specifiche su importi/divise
155000160908     C                   EXSR      CHKIMPDIV
155100160908     C*
155200160908     C                   ENDSR
155300160908     C***
155400010601
155500020204
155600020204     C*----------------------------------------------------*
155700020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
155800020204     C*----------------------------------------------------*
155900020204     C     CHKIMPDIV     BEGSR
156000060524     C*
156100060524     C* Verifico il codice bolla in base al tipo porto e alla presenza o meno del contrassegno
156200160906     C                   IF        vabCAS  > *zeros OR
156300160906     C                             wFlgCAS > *zeros
156400060524     C                   IF        vabCBO = '1'
156500060524     C                   EVAL      vabCBO = '4'
156600060524     C                   ENDIF
156700060524     C                   IF        vabCBO = '2'
156800060524     C                   EVAL      vabCBO = '6'
156900060524     C                   ENDIF
157000060524     C                   ENDIF
157100020204     C*
157200020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
157300020204     C                   Z-ADD     *zeros        wrkDec            9 9
157400020204     C*
157500020204     C* Come prima cosa effettuo considerazioni sulla divisa
157600020204     C                   IF        vabIAS > *zeros
157700150420     C                   IF        vabVAS =  *blanks
157800150420     C                   EVAL      vabVAS =  'EUR'
157900020204     C                   ENDIF
158000020204     C                   ENDIF
158100020204     C*
158200020204     C                   IF        vabCAS > *zeros
158300150420     C                   IF        vabVCA <> *blanks
158400150420     C                   EVAL      vabVCA =  'EUR'
158500020204     C                   ENDIF
158600020204     C                   ENDIF
158700020204     C*
158800020204     C                   IF        vabVMD > *zeros
158900150420     C                   IF        vabVAD <> *blanks
159000150420     C                   EVAL      vabVAD =  'EUR'
159100020204     C                   ENDIF
159200020204     C                   ENDIF
159300020204     C*
159400020204     C                   ENDSR
159500020204     C***
159600010330
159700010330
159800010330     C*----------------------------------------------------*
159900010330     C*  CONTROLLO NUMERICITA' CAMPI
160000010330     C*----------------------------------------------------*
160100010330     C     CHKNUM        BEGSR
160200010330     C*
160300010606     C                   IF        PiDecChr = *blanks
160400030715     C                   EVAL      PiDecChr = CharNUM
160500010606     C                   ENDIF
160600010606     C*
160700010606     C                   CALL(e)   'ISNUMERIC'
160800010330     C                   PARM                    PiStr            30
160900010606     C                   PARM                    PiDecChr          1
161000010330     C                   PARM      *ZEROS        PiVal            30 9
161100010330     C                   PARM      '0'           PiInt             1
161200010330     C                   PARM      '0'           PiNum             1
161300010330     C                   IF        %error
161400010606     C                   EVAL      PiNum=*off
161500010330     C                   ENDIF
161600010330     C*
161700010330     C                   ENDSR
161800010330     C***
161900160906
162000160906
162100160906      /TITLE Valorizzazione Progressivo Applicazione
162200160906     C     calprog       begsr
162300160906     C*
162400160906     C                   movel     *blanks       dwlisv            2
162500160906     C                   movel     *all'0'       dwlprg           10
162600160906     C                   z-add     *zeros        wrkprg            8 0
162700160906     C*
162800160906     C                   eval      dwlisv = vlrisv
162900160906     C*
163000170324     C     *start        setll     tis7prgf
163100160906     C                   read(e)   tis7prgf
163200160906     C                   if        not %error
163300160906     C                   eval      dwlprg = f_tis7prgf
163400160906     C*
163500160906     C                   move(p)   dwlprg        wrkprg
163600160906     C                   add       1             wrkprg
163700160906     C                   move(p)   wrkprg        dwlprg
163800160906     C                   movel     dwlisv        dwlprg
163900160906     C*
164000160906     C                   eval      f_tis7prgf = dwlprg
164100160906     C                   update    tis7prg0
164200170324     C                   else
164300170324     C                   eval      f_tis7prgf = 'XX00000000'
164400160906     C                   endif
164500160906     C*
164600160906     C                   endsr
164700050421
164800160908
164900160908     C     *pssr         BEGSR
165000160908     C*
165100160908     C                   if        %open(tivin00r)
165200160908     C                   close     tivin00r
165300160908     C                   endif
165400160908     C                   if        %open(fivabwwr)
165500160908     C                   close     fivabwwr
165600160908     C                   endif
165700160908     C                   if        %open(fivatwwr)
165800160908     C                   close     fivatwwr
165900160908     C                   endif
166000160908     C*
166100160908     C* Effettuo la chiamata al CLLE preposto
166200160908     C  N50              call(e)   'TITVVTC'
166300160908     C                   parm                    parccm
166400160908     C                   parm                    parmbr
166500160908     C                   parm      '2'           paropz
166600160908     C*
166700160908     C                   eval      wrkesito = '2'
166800160908     C*
166900160908     C                   seton                                        LR
167000160908     C*
167100160908     C                   ENDSR     '*CANCL'
167200160908     C***
167300100419
167400100419
167500160902     C     normCSV       BEGSR
167600160902     C*
167700160902     C                   z-add     *zeros        wLenVINDTA        4 0
167800160902     C                   z-add     *zeros        wFlgVINDTA        1 0
167900160902     C                   setoff                                       80
168000160902     C*
168100160902     C                   dow       wLenVINDTA < %len(vindta)
168200160902     C                   eval      wLenVINDTA = wLenVINDTA + 1
168300160902     C                   movel     *blanks       wAlfa1            1
168400160902     C                   eval      wAlfa1 = %subst(vindta:wLenVINDTA:1)
168500160902     C*
168600160902     C* Se byte inferiore a HEX40 è un byte "non stampabile" per cui considero *blank
168700170315     C                   if        wAlfa1 <  X'40' AND
168800170315     C                             wAlfa1 <> CharCSV
168900160902     C                   eval      %subst(vindta:wLenVINDTA:1) = *blanks
169000160902     C                   endif
169100160902     C*
169200160902     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
169300160902     C                   if        wAlfa1 <> CharCSV and
169400160902     C                             wAlfa1 <> CharTXT and
169500160902     C                             wAlfa1 <> CharNUM
169600160902     C                   z-add     1             wFlgVINDTA
169700160902     C                   endif
169800160902     C*
169900160902     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
170000160902     C* separatori campi
170100160902     C                   if        CharTXT <> *blanks
170200160902     C                   if        wAlfa1 = CharTXT
170300160902     C                   if        not *in80
170400160902     C                   seton                                        80
170500160902     C                   else
170600160902     C                   setoff                                       80
170700160902     C                   endif
170800160902     C                   endif
170900160902     C                   if        wAlfa1 = CharCSV and *in80 = *on
171000160902     C                   eval      %subst(vindta:wLenVINDTA:1) = CharSOS
171100160902     C                   endif
171200160902     C                   endif
171300160902     C*
171400160902     C                   enddo
171500100419     C*
171600100419     C                   ENDSR
171700100419     C***
171800150420
171900150420
172000150420      /TITLE Scrittura record in file TIVGD00F (file VAS generico download)
172100160902     C     wriVAB_VGD    BEGSR
172200160909     C*
172300160909     C* Verifica campi "particolari" VAB
172400160909     C                   exsr      repNSP
172500160909     C*
172600160909     C                   setoff                                       45
172700160909     C                   if        depSPE  = *zeros OR
172800160909     C                             depSPE <> vabNSP
172900160909     C                   seton                                        45
173000160909     C                   eval      depSPE  = vabNSP
173100160909     C                   endif
173200160909     C*
173300160909     C                   if        not *in31
173400150420     C*
173500160902     C* Se primo giro allora scrivo testata
173600160902     C                   if        wVGD_Head = *zeros
173700160902     C                   eval      wVGD_Head = 1
173800160902     C                   eval      wBufferOut =
173900170316     C                             %trim(pptCHR)+'VABATB' +%trim(pptCHR)+pptCSV+
174000160902     C                             %trim(pptCHR)+'VABCCM' +%trim(pptCHR)+pptCSV+
174100160902     C                             %trim(pptCHR)+'VABLNP' +%trim(pptCHR)+pptCSV+
174200160902     C                             %trim(pptCHR)+'VABCTR' +%trim(pptCHR)+pptCSV+
174300160902     C                             %trim(pptCHR)+'VABRSD' +%trim(pptCHR)+pptCSV+
174400160902     C                             %trim(pptCHR)+'VABRD2' +%trim(pptCHR)+pptCSV+
174500160902     C                             %trim(pptCHR)+'VABIND' +%trim(pptCHR)+pptCSV+
174600160902     C                             %trim(pptCHR)+'VABLOD' +%trim(pptCHR)+pptCSV+
174700160902     C                             %trim(pptCHR)+'VABCAD' +%trim(pptCHR)+pptCSV+
174800160902     C                             %trim(pptCHR)+'VABPRD' +%trim(pptCHR)+pptCSV+
174900160902     C                             %trim(pptCHR)+'VABCBO' +%trim(pptCHR)+pptCSV+
175000160902     C                             %trim(pptCHR)+'VABRMO' +%trim(pptCHR)+pptCSV+
175100160902     C                             %trim(pptCHR)+'VABRMN' +%trim(pptCHR)+pptCSV+
175200160902     C                             %trim(pptCHR)+'VABRMA' +%trim(pptCHR)+pptCSV+
175300160902     C                             %trim(pptCHR)+'VABNAS' +%trim(pptCHR)+pptCSV+
175400160902     C                             %trim(pptCHR)+'VABNCL' +%trim(pptCHR)+pptCSV+
175500160902     C                             %trim(pptCHR)+'VABPKB' +%trim(pptCHR)+pptCSV+
175600160902     C                             %trim(pptCHR)+'VABCAS' +%trim(pptCHR)+pptCSV+
175700160902     C                             %trim(pptCHR)+'VABVCA' +%trim(pptCHR)+pptCSV+
175800160902     C                             %trim(pptCHR)+'VABFFD' +%trim(pptCHR)+pptCSV+
175900160902     C                             %trim(pptCHR)+'VABIAS' +%trim(pptCHR)+pptCSV+
176000160902     C                             %trim(pptCHR)+'VABVAS' +%trim(pptCHR)+pptCSV+
176100160902     C                             %trim(pptCHR)+'VABNOT' +%trim(pptCHR)+pptCSV+
176200160902     C                             %trim(pptCHR)+'VABNRC' +%trim(pptCHR)+pptCSV+
176300160902     C                             %trim(pptCHR)+'VABTRC' +%trim(pptCHR)+pptCSV+
176400160902     C                             %trim(pptCHR)+'VABEMD' +%trim(pptCHR)+pptCSV+
176500160902     C                             %trim(pptCHR)+'VABCEL' +%trim(pptCHR)
176600160902     C                   exsr      wriVGD
176700160902     C                   endif
176800160902     C*
176900160902     C* Quindi scrivo dati spedizione
177000160909     C   45              eval      wBufferOut =
177100170316     C                             %trim(pptCHR)+%trim(VABATB)
177200170316     C                                                    +%trim(pptCHR)+pptCSV+
177300150420     C                             %trim(pptCHR)+%trim(%editc(VABCCM:'X'))
177400150420     C                                                    +%trim(pptCHR)+pptCSV+
177500150420     C                             %trim(pptCHR)+%trim(%editc(VABLNP:'X'))
177600150420     C                                                    +%trim(pptCHR)+pptCSV+
177700150420     C                             %trim(pptCHR)+%trim(%editc(VABCTR:'X'))
177800150420     C                                                    +%trim(pptCHR)+pptCSV+
177900150420     C                             %trim(pptCHR)+%trim(VABRSD)
178000150420     C                                                    +%trim(pptCHR)+pptCSV+
178100160902     C                             %trim(pptCHR)+%trim(VABRD2)
178200160902     C                                                    +%trim(pptCHR)+pptCSV+
178300150420     C                             %trim(pptCHR)+%trim(VABIND)
178400150420     C                                                    +%trim(pptCHR)+pptCSV+
178500150420     C                             %trim(pptCHR)+%trim(VABLOD)
178600150420     C                                                    +%trim(pptCHR)+pptCSV+
178700150420     C                             %trim(pptCHR)+%trim(VABCAD)
178800150420     C                                                    +%trim(pptCHR)+pptCSV+
178900150420     C                             %trim(pptCHR)+%trim(VABPRD)
179000150420     C                                                    +%trim(pptCHR)+pptCSV+
179100150420     C                             %trim(pptCHR)+%trim(VABCBO)
179200150420     C                                                    +%trim(pptCHR)+pptCSV+
179300150420     C                             %trim(pptCHR)+%trim(VABRMO)
179400150420     C                                                    +%trim(pptCHR)+pptCSV+
179500150420     C                             %trim(pptCHR)+%trim(%editc(VABRMN:'4'))
179600150420     C                                                    +%trim(pptCHR)+pptCSV+
179700150420     C                             %trim(pptCHR)+%trim(VABRMA)
179800150420     C                                                    +%trim(pptCHR)+pptCSV+
179900150420     C                             %trim(pptCHR)+%trim(VABNAS)
180000150420     C                                                    +%trim(pptCHR)+pptCSV+
180100150420     C                             %trim(pptCHR)+%trim(%editc(VABNCL:'4'))
180200150420     C                                                    +%trim(pptCHR)+pptCSV+
180300150420     C                             %trim(pptCHR)+%trim(%editc(VABPKB:'4'))
180400150420     C                                                    +%trim(pptCHR)+pptCSV+
180500150420     C                             %trim(pptCHR)+%trim(%editc(VABCAS:'4'))
180600150420     C                                                    +%trim(pptCHR)+pptCSV+
180700150420     C                             %trim(pptCHR)+%trim(VABVCA)
180800150420     C                                                    +%trim(pptCHR)+pptCSV+
180900150420     C                             %trim(pptCHR)+%trim(VABFFD)
181000150420     C                                                    +%trim(pptCHR)+pptCSV+
181100150420     C                             %trim(pptCHR)+%trim(%editc(VABIAS:'4'))
181200150420     C                                                    +%trim(pptCHR)+pptCSV+
181300150420     C                             %trim(pptCHR)+%trim(VABVAS)
181400150420     C                                                    +%trim(pptCHR)+pptCSV+
181500150421     C                             %trim(pptCHR)+%trim(VABNOT)
181600150421     C                                                    +%trim(pptCHR)+pptCSV+
181700160902     C                             %trim(pptCHR)+%trim(wVATNOT_A)
181800160902     C                                                    +%trim(pptCHR)+pptCSV+
181900160902     C                             %trim(pptCHR)+%trim(wVATNOT_B)
182000160902     C                                                    +%trim(pptCHR)+pptCSV+
182100150420     C                             %trim(pptCHR)+%trim(wVATNOT_I)
182200150420     C                                                    +%trim(pptCHR)+pptCSV+
182300150420     C                             %trim(pptCHR)+%trim(wVATNOT_S)
182400150420     C                                                    +%trim(pptCHR)
182500160909     C   45              exsr      wriVGD
182600150420     C*
182700160909     C                   endif
182800160909     C*
182900150420     C                   ENDSR
183000150420     C*------------------------------------------------------------------------*
183100150420
183200150420
183300150420
183400150420      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
183500150420     C     wriVGD        BEGSR
183600150420     C*
183700150420     C                   clear                   tivgd000
183800150420     C                   eval      vgdDTA = wBufferOut
183900150420     C                   eval      vgdTIP = 'EB'
184000150420     C                   eval      vgdKSU = vlrKSC
184100150420     C                   eval      vgdTSC = 'WW'
184200150420     C                   eval      vgdDAT = datcor
184300160906     C                   eval      vgdPRG = f_tis7prgf
184400160920     C                   eval      vgdPGM = procname
184500160906     C                   eval      vgdSTO = '?'
184600150420     C                   write     tivgd000
184700150420     C*
184800150420     C                   ENDSR
184900150420     C*------------------------------------------------------------------------*
185000160902
185100160902
185200160902     C*----------------------------------------------------*
185300160902     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
185400160902     C*----------------------------------------------------*
185500160902     C     SR_SPLIT      BEGSR
185600160902     C*
185700160902     C* Chiamo il *pgm d utilità x lo "split" della riga delle intestazioni
185800160902     C                   CALL      'XSPLIT2'
185900160902     C                   PARM                    InStringa
186000160902     C                   PARM                    InSepara
186100160902     C                   PARM      '5'           InTypeOut
186200160902     C                   PARM      ''            wSkParam
186300160902     C                   PARM                    OutErrore
186400160902     C                   PARM                    OutMsg
186500160902     C                   MOVEA     wSkParam      SkSplitCSV_5
186600160902     C*
186700160902     C                   ENDSR
186800170316
186900170316
187000170316
187100170316     C*--------------------------------------------------------
187200170316     C* CARTAB - CARICAMENTO DATI TABELLARI                   *
187300170316     C*--------------------------------------------------------
187400170316     C     CARTAB        BEGSR
187500170316     C*
187600170316     C* TABELLA '15' - NAZIONI
187700170316     C                   clear                   skNAZISO
187800170316     C                   clear                   skNAZBAR
187900170316     C                   eval      tblKUT = 1
188000170316     C                   eval      tblCOD = '15'
188100170316     C     KEYtabP       setll     tabel00f
188200170316     C     KEYtabP       reade     tabel00f
188300170316     C                   dow       not %eof(tabel00f)
188400170316     C                   if        tblFLG = *blanks
188500170316     C                   movel(p)  tblUNI        ds15
188600170316     C                   add       1             jNAZ
188700170316     C                   eval      skNAZBAR(jNAZ) = tblKEY
188800170316     C                   eval      skNAZISO(jNAZ) = §15ISO
188900170316     C                   endif
189000170316     C     KEYtabP       reade     tabel00f
189100170316     C                   enddo
189200170316     C*
189300170316     C                   ENDSR
189400170316     C***
189500170316
189600150420
189700150420
189800000613     C     *inzsr        BEGSR
189900990910     C*
190000990910     C     *entry        plist
190100990920     C                   parm                    tivlrds
190200990921     C                   parm      wrkesito      esito
190300000724     C                   parm                    prmlit
190400000710     C                   parm                    prmfir
190500010330     C*
190600010330     C* CALCOLA LA DATA CORRENTE
190700150420     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
190800150420     C                   eval      datcor = %dec(%date() : *ISO)
190900170316     C*
191000170316     C* Chiave su TABEL00F - parziale
191100170316     C     KEYtabP       KLIST
191200170316     C                   KFLD                    tblKUT
191300170316     C                   KFLD                    tblCOD
191400000613     C*
191500000613     C                   ENDSR
191600000613     C***
