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
021800170919     C                             %len(%trim(SkSplitCSV_5(19))) > 3 and
021900170919     C                             %len(%trim(SkSplitCSV_5(20))) > 3 and
022000170315     C                             %len(%trim(SkSplitCSV_5(23))) > 2
022100160902     C*****************************
022200010601     C*
022300160902     C* Se OK => procedo con elaborazione
022400010330     C                   exsr      inzvar
022500010605     C                   exsr      impvab
022600010601     C*
022700010604     C* Ebbene...
022800160908     C                   if        wGiro > 1
022900160908     C  N31              add       1             §CTROKVB
023000160908     C   32              add       1             §CTRMO
023100160908     C   31              add       1             §CTRNO
023200160908     C*
023300160902     C   50              exsr      wriVAB_VGD
023400160908     C  N50              exsr      wriVAB_WW
023500150420     C                   endif
023600010604     C*
023700010604     C                   if        *in31 = *off and
023800010604     C                             *in32 = *off
023900010604     C                   eval      vinflg = '1'
024000010604     C                   else
024100010604     C                   eval      vinflg = '2'
024200010604     C                   endif
024300010601     C*
024400160902     C                   else
024500160902     C                   eval      vinflg = '1'
024600010604     C                   endif
024700160902     C*
024800160902     C                   else
024900160902     C                   eval      vinflg = '1'
025000160902     C                   endif
025100010604     C*
025200010330     C                   else
025300010330     C                   eval      vinflg = '1'
025400010330     C                   endif
025500010601     C*
025600160902     C                   eval      vindta = s_vindta
025700010601     C  N70              update    tivin000
025800010330     C*
025900030822     C  N70              enddo
026000010601     C
026100010601     C*
026200160902     C* Stabilisco l'esito della traduzione
026300160902     C                   select
026400160902     C*
026500160902     C* Se non ci sono record con errori => stato OK
026600160902     C                   when      §ctrno = 0 and
026700160902     C                             §ctrmo = 0
026800010330     C                   eval      wrkesito = '0'
026900160902     C*
027000160902     C* Se nessun record OK => errore totale
027100160908     C                   when      §ctrokvb = 0
027200160902     C                   eval      wrkesito = '2'
027300160902     C*
027400160902     C* Diversamente => errore parziale/warning
027500160902     C                   other
027600160902     C                   eval      wrkesito = '1'
027700160902     C*
027800160902     C                   endsl
027900160908     C*
028000160908     C                   endif
028100160908     C
028200160908     C                   if        chkcall = '1'
028300160908     C                   eval      wrkesito = '2'
028400160908     C                   endif
028500010330     C*
028600010330     C                   if        %open(tivin00r)
028700010330     C                   close     tivin00r
028800010330     C                   endif
028900160908     C*
029000160908     C                   if        %open(fivabwwr)
029100160908     C                   close     fivabwwr
029200160908     C                   endif
029300160908     C                   if        %open(fivatwwr)
029400160908     C                   close     fivatwwr
029500160908     C                   endif
029600160908     C*
029700160908     C                   if        not *in50        and
029800160908     C                             wrkesito <> '2'  and
029900160908     C                             vlrpoi <> *zeros
030000160908     C                   exsr      invio
030100160908     C                   endif
030200010330     C*
030300010330     C                   ENDSR
030400010330     C***
030500160908
030600160908
030700160908     C*----------------------------------------------------*
030800160908     C*  SCARICAMENTO BUFFER RECORDS VAB IN "WW"
030900160908     C*----------------------------------------------------*
031000160908     C     WRIVAB_WW     BEGSR
031100160908     C*
031200160908     C* Verifica campi "particolari" VAB
031300160908     C                   exsr      repNSP
031400160909     C*
031500160909     C                   setoff                                       45
031600160909     C                   if        depSPE  = *zeros OR
031700160909     C                             depSPE <> vabNSP
031800160909     C                   seton                                        45
031900160909     C                   eval      depSPE  = vabNSP
032000160909     C                   endif
032100160909     C*
032200160909     C                   if        not *in31
032300160909     C                   exsr      EXEVATE
032400160909     C   45              exsr      EXEVATA
032500160909     C   45              exsr      EXEVATB
032600160909     C   45              exsr      EXEVATI
032700160909     C   45              exsr      EXEVATJ
032800160909     C   45              exsr      EXEVATS
032900160909     C   45              write     fivab000                                     => scarico il VAB
033000160909     C                   endif
033100160908     C*
033200160908     C                   ENDSR
033300160908
033400160908
033500160908     C*----------------------------------------------------*
033600160908     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
033700160908     C*----------------------------------------------------*
033800160908     C     REPNSP        BEGSR
033900160908     C*
034000160909     C* NSP => Se necessario stacco un numeratore da AZNUM
034100160909     C                   if        VABNSP = *zeros
034200160908     C                   clear                   TRUL33DS
034300160908     C                   eval      I33OPE = *zeros
034400160908     C                   eval      I33CNU = 302
034500160908     C                   eval      I33NUM = 1
034600160908     C                   movel     TRUL33DS      KPJBU
034700160908     C                   call      'TRUL33R'
034800160908     C                   parm                    KPJBA
034900160908     C                   movel     KPJBU         TRUL33DS
035000160908     C                   if        O33ERR = *zeros
035100160908     C                   z-add     O33NRF        VABNSP
035200160908     C                   z-add     O33NRF        VATNSP
035300160908     C                   else
035400160908     C                   SETON                                        31
035500160908     C                   EVAL      vinmsg = %trimr(vinmsg)
035600160908     C                             + ' ' + 'VABNSP VATNSP'
035700160908     C                   endif
035800160909     C                   endif
035900160908     C*
036000160908     C                   ENDSR
036100160908
036200160908
036300160908     C*----------------------------------------------------*
036400160908     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
036500160908     C*----------------------------------------------------*
036600160908     C     EXEVATA       BEGSR
036700160908     C*
036800160908     C                   EVAL      VATTRC = 'A'
036900160908     C                   EVAL      VATNOT = wVATNOT_A
037000160908     C                   exsr      wrivat                                       => scarico VAT
037100160908     C*
037200160908     C                   ENDSR
037300160908     C*----------------------------------------------------*
037400160908     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
037500160908     C*----------------------------------------------------*
037600160908     C     EXEVATB       BEGSR
037700160908     C*
037800160908     C                   EVAL      VATTRC = 'B'
037900160908     C                   EVAL      VATNOT = wVATNOT_B
038000160908     C                   exsr      wrivat                                       => scarico VAT
038100160908     C*
038200160908     C                   ENDSR
038300160909     C*----------------------------------------------------*
038400160909     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "I"
038500160909     C*----------------------------------------------------*
038600160909     C     EXEVATI       BEGSR
038700160909     C*
038800160909     C                   IF        %len(%trim(wVATNOT_I)) > 35
038900160909     C                   EVAL      VATTRC = 'I'
039000160909     C                   EVAL      VATNOT = %subst(%trim(wVATNOT_I):1:35)
039100160909     C                   exsr      wrivat                                       => scarico VAT
039200160909     C                   EVAL      VATTRC = 'J'
039300160909     C                   EVAL      VATNOT = %subst(%trim(wVATNOT_I):36)
039400160909     C                   exsr      wrivat                                       => scarico VAT
039500160909     C                   ELSE
039600160909     C                   EVAL      VATTRC = 'I'
039700160909     C                   EVAL      VATNOT = wVATNOT_I
039800160909     C                   exsr      wrivat                                       => scarico VAT
039900160909     C                   ENDIF
040000160909     C*
040100160909     C                   ENDSR
040200160909     C*----------------------------------------------------*
040300160909     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "J"
040400160909     C*----------------------------------------------------*
040500160909     C     EXEVATJ       BEGSR
040600160909     C*
040700160909     C                   IF        %len(%trim(wVATNOT_I)) <= 35
040800160909     C                   EVAL      VATTRC = 'J'
040900160909     C                   EVAL      VATNOT = wVATNOT_J
041000160909     C                   exsr      wrivat                                       => scarico VAT
041100160909     C                   ENDIF
041200160909     C*
041300160909     C                   ENDSR
041400160908     C*----------------------------------------------------*
041500160908     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "S"
041600160908     C*----------------------------------------------------*
041700160908     C     EXEVATS       BEGSR
041800160908     C*
041900160908     C                   EVAL      VATTRC = 'S'
042000160908     C                   EVAL      VATNOT = wVATNOT_S
042100160908     C                   exsr      wrivat                                       => scarico VAT
042200160908     C*
042300160908     C                   ENDSR
042400160908     C*----------------------------------------------------*
042500160908     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
042600160908     C*----------------------------------------------------*
042700160908     C     EXEVATE       BEGSR
042800160908     C*
042900160908     C                   EVAL      VATTRC = 'E'
043000160908     C                   EVAL      VATNOT = wVATNOT_E
043100160908     C                   exsr      wrivat                                       => scarico VAT
043200160908     C*
043300160908     C                   ENDSR
043400160908     C*----------------------------------------------------*
043500160908     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
043600160908     C*----------------------------------------------------*
043700160908     C     WRIVAT        BEGSR
043800160908     C*
043900160908     C* Scrivo solo se valorizzato qualcosa
044000160908     C                   IF        VATNOT <> *blanks
044100160908     C                   EVAL      VATFGS = VABFGS
044200160908     C                   EVAL      VATCCM = VABCCM
044300160908     C                   EVAL      VATLNP = VABLNP
044400160908     C                   EVAL      VATAAS = VABAAS
044500160908     C                   EVAL      VATNRS = VABNRS
044600160908     C                   EVAL      VATNSP = VABNSP
044700160908     C                   WRITE     FIVAT000
044800160908     C                   ADD       1             §CTROKVT
044900160908     C                   ENDIF
045000160908     C*
045100160908     C                   ENDSR
045200160908
045300160908
045400160908     C*----------------------------------------------------*
045500160908     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE ACCESSORIO
045600160908     C*----------------------------------------------------*
045700160908     C     PREVAT        BEGSR
045800160908     C*
045900160908     C* Compongo il nome del membro da dare al FIVATWWR
046000160908     C                   eval      parmbr = vlrhdl
046100160908     C                   movel     'M'           parmbr
046200160908     C                   eval      parccm = %subst(vlrKSC:2:7)
046300160908     C                   eval      paropz = '1'
046400160908     C*
046500160908     C* Effettuo la chiamata al CLLE preposto
046600160908     C                   call(e)   'TITVVTC'
046700160908     C                   parm                    parccm
046800160908     C                   parm                    parmbr
046900160908     C                   parm                    paropz
047000160908     C*
047100160908     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
047200160908     C                   if        %error
047300160908     C                   movel     '1'           chkcall
047400160908     C                   else
047500160908     C                   movel     '0'           chkcall
047600160908     C                   endif
047700160908     C*
047800160908     C                   ENDSR
047900160908
048000160908
048100160908      /TITLE Invio dei dati al punto operativo.
048200160908     C     invio         BEGSR
048300160908     C*
048400160908     C* 1° invio FIVAT
048500160908     C                   reset                   dscmz
048600160908     C                   move      vlrpoi        cmzdst
048700160908     C                   eval      cmzfld = 'FIVATWWR'
048800160908     C                   eval      cmzmbd = vlrhdl
048900160908     C                   eval      %subst(cmzmbd:1:1) = 'M'
049000160908     C***                if        prmfir = *blanks
049100160908     C                   eval      cmzfla = 'FIVAT00F'
049200160908     C                   eval      cmzmba = 'FIVAT00F'
049300160908     C***                else
049400160908     C***                eval      cmzfla = prmfir
049500160908     C***                eval      cmzmba = prmfir
049600160908     C***                endif
049700160908     C                   eval      cmznrr = *zeros
049800160908     C                   move      §ctrokvt      cmznrr
049900160908     C                   eval      cmzlba = vlrfl1
050000160908     C                   call(e)   'TIS711C'
050100160908     C                   parm                    dscmz
050200160908     C                   parm      *blanks       esito
050300160908     C                   if        %error
050400160908     C                             or cmzerr = '1'
050500160908     C                             or esito  = '1'
050600160908     C                   eval      wrkesito = '3'
050700160908     C                   else
050800160908     C*
050900160908     C* 2° invio FIVAB
051000160908     C                   reset                   dscmz
051100160908     C                   move      vlrpoi        cmzdst
051200160908     C                   eval      cmzfld = vlrfou
051300160908     C                   eval      cmzmbd = vlrhdl
051400160908     C                   eval      %subst(cmzmbd:1:1) = 'M'
051500160908     C***                if        prmfir = *blanks
051600160908     C                   eval      cmzfla = 'FIVAB00F'
051700160908     C                   eval      cmzmba = 'FIVAB00F'
051800160908     C***                else
051900160908     C***                eval      cmzfla = prmfir
052000160908     C***                eval      cmzmba = prmfir
052100160908     C***                endif
052200160908     C                   eval      cmznrr = *zeros
052300160908     C                   move      §ctrokvb      cmznrr
052400160908     C                   eval      cmzlba = vlrfl1
052500160908     C                   call(e)   'TIS711C'
052600160908     C                   parm                    dscmz
052700160908     C                   parm      *blanks       esito
052800160908     C                   if        %error
052900160908     C                             or cmzerr = '1'
053000160908     C                             or esito  = '1'
053100160908     C                   eval      wrkesito = '3'
053200160908     C                   endif
053300160908     C                   endif
053400160908     C*
053500160908     C                   ENDSR
053600160908     C***
053700010601
053800010601
053900010330     C*----------------------------------------------------*
054000030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
054100010330     C*----------------------------------------------------*
054200010330     C     INZVAR        BEGSR
054300010330     C*
054400030822     C* Inizializzo variabili di wrk
054500160902     C                   Z-ADD     *zeros        Num5_0            5 0
054600030822     C*
054700030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
054800160908     C                   CLEAR                   FIVAB000
054900160908     C                   CLEAR                   FIVAT000
055000170322     C                   CLEAR                   wVABRMA_1
055100170322     C                   CLEAR                   wVABRMA_2
055200160908     C                   CLEAR                   wVABNOT
055300160902     C                   CLEAR                   wVATNOT_A
055400160902     C                   CLEAR                   wVATNOT_B
055500160902     C                   CLEAR                   wVATNOT_E
055600150420     C                   CLEAR                   wVATNOT_I
055700160908     C                   CLEAR                   wVATNOT_J
055800150420     C                   CLEAR                   wVATNOT_S
055900030822     C                   CLEAR                   SkSplitCSV
056000160902     C                   CLEAR                   wFlgCAS
056100030822     C                   EVAL      i = 1
056200030822     C*
056300030822     C* Reimposto i valori di default
056400030822     C                   EXSR      DEFCAM
056500030822     C*
056600010330     C                   ENDSR
056700160902
056800160902
056900010330     C*----------------------------------------------------*
057000030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
057100010330     C*----------------------------------------------------*
057200010330     C     DEFCAM        BEGSR
057300110706     C*
057400160906     C                   SETOFF                                       7172
057500030822     C*
057600030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
057700030715     C* e delimitatore testo.
057800030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
057900170315     C*
058000170315     C* Verifico eventuali valori particolari per il carattere separatore campo
058100170315     C                   IF        CharCSV = '5'
058200170315     C                   EVAL      CharCSV = x'05'
058300170315     C                   ENDIF
058400170315     C*
058500030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
058600030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
058700150420     C                   IF        %subst(vlrppt:5:1) <> *blanks
058800150420     C                   EVAL      pptCSV = %subst(vlrppt:5:1)
058900150420     C                   ELSE
059000150420     C                   EVAL      pptCSV = ';'
059100150420     C                   ENDIF
059200150420     C                   IF        %subst(vlrppt:6:1) <> *blanks
059300150420     C                   EVAL      pptCHR = %subst(vlrppt:6:1)
059400150420     C                   ELSE
059500150420     C                   EVAL      pptCHR = ' '
059600150420     C                   ENDIF
059700090403     C*
059800090403     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
059900160902     C                   EVAL      CharSOS = CharNUM
060000030822     C*
060100030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
060200030715     C                   IF        %subst(vlrppt:1:1) = 'S'
060300160902     C                   SETON                                        50        * OUT => Download
060400030715     C                   ELSE
060500160902     C                   SETOFF                                       50        * OUT => File WW
060600030715     C                   ENDIF
060700030822     C*
060800030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
060900030822     C                   EVAL      posDaDft = 1
061000030822     C                   EVAL      posADft  = 0
061100030822     C                   EVAL      wGiroDft = 0
061200030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
061300030822     C                             posDaDft > 0
061400030822     C*
061500030822     C* Gestisco il 1° giro
061600030822     C                   IF        wGiroDft = 0
061700030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
061800030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
061900030822     C* Incremento il contatore dei "giri"
062000030822     C                   EVAL      wGiroDft = 1
062100030822     C                   ELSE
062200030822     C                   EVAL      posDaDft = posADft
062300030822     C                   ENDIF
062400030822     C* Eseguo lo scan x trovare la fine del campo corrente
062500030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
062600030822     C*
062700030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
062800030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
062900030822     C                   IF        posDaDft > 0 AND
063000030822     C                             posADft  > 0
063100110706     C* RMN
063200110706     C                   IF        %subst(
063300110706     C                             %subst(vlrppt:posDaDft+1:
063400110706     C                             posADft-posDaDft-1):1:3)
063500110706     C                             = 'RMN'
063600110706     C                   IF        %trim(%subst(
063700110706     C                             %subst(vlrppt:posDaDft+1:
063800110706     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063900110706     C                             = '*NSP'
064000160902     C                   SETON                                        71
064100110706     C                   ENDIF
064200110706     C                   ENDIF
064300060331     C* FGS
064400060331     C                   IF        %subst(
064500060331     C                             %subst(vlrppt:posDaDft+1:
064600060331     C                             posADft-posDaDft-1):1:3)
064700060331     C                             = 'FGS'
064800060331     C                   EVAL      PiStr=%trim(%subst(
064900060331     C                             %subst(vlrppt:posDaDft+1:
065000060331     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065100060331     C                   EXSR      CHKNUM
065200060331     C                   IF        PiInt=*on
065300060331     C                   Z-ADD     PiVal         VABFGS
065400060331     C                   ENDIF
065500060331     C                   ENDIF
065600030822     C* NCL
065700030822     C                   IF        %subst(
065800030822     C                             %subst(vlrppt:posDaDft+1:
065900030822     C                             posADft-posDaDft-1):1:3)
066000030822     C                             = 'NCL'
066100030822     C                   EVAL      PiStr=%trim(%subst(
066200030822     C                             %subst(vlrppt:posDaDft+1:
066300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066400030822     C                   EXSR      CHKNUM
066500030822     C                   IF        PiInt=*on
066600030822     C                   Z-ADD     PiVal         VABNCL
066700030822     C                   ENDIF
066800030822     C                   ENDIF
066900030822     C* CCM
067000030822     C                   IF        %subst(
067100030822     C                             %subst(vlrppt:posDaDft+1:
067200030822     C                             posADft-posDaDft-1):1:3)
067300030822     C                             = 'CCM'
067400030822     C                   EVAL      PiStr=%trim(%subst(
067500030822     C                             %subst(vlrppt:posDaDft+1:
067600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067700030822     C                   EXSR      CHKNUM
067800030822     C                   IF        PiInt=*on
067900030822     C                   Z-ADD     PiVal         VABCCM
068000030822     C                   ENDIF
068100030822     C                   ENDIF
068200030822     C* LNP
068300030822     C                   IF        %subst(
068400030822     C                             %subst(vlrppt:posDaDft+1:
068500030822     C                             posADft-posDaDft-1):1:3)
068600030822     C                             = 'LNP'
068700030822     C                   EVAL      PiStr=%trim(%subst(
068800030822     C                             %subst(vlrppt:posDaDft+1:
068900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069000030822     C                   EXSR      CHKNUM
069100030822     C                   IF        PiInt=*on
069200030822     C                   Z-ADD     PiVal         VABLNP
069300030822     C                   ENDIF
069400030822     C                   ENDIF
069500030822     C* NRS
069600030822     C                   IF        %subst(
069700030822     C                             %subst(vlrppt:posDaDft+1:
069800030822     C                             posADft-posDaDft-1):1:3)
069900030822     C                             = 'NRS'
070000030822     C                   EVAL      PiStr=%trim(%subst(
070100030822     C                             %subst(vlrppt:posDaDft+1:
070200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070300030822     C                   EXSR      CHKNUM
070400030822     C                   IF        PiInt=*on
070500030822     C                   Z-ADD     PiVal         VABNRS
070600030822     C                   ENDIF
070700030822     C                   ENDIF
070800160906     C* CTR
070900030822     C                   IF        %subst(
071000030822     C                             %subst(vlrppt:posDaDft+1:
071100030822     C                             posADft-posDaDft-1):1:3)
071200030822     C                             = 'CTR'
071300030822     C                   EVAL      PiStr=%trim(%subst(
071400030822     C                             %subst(vlrppt:posDaDft+1:
071500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071600030822     C                   EXSR      CHKNUM
071700030822     C                   IF        PiInt=*on
071800160906     C                   Z-ADD     PiVal         VABCTR
071900030822     C                   ENDIF
072000030822     C                   ENDIF
072100160906     C* CTE (ovvero CTR per Estero)
072200160906     C                   IF        %subst(
072300160906     C                             %subst(vlrppt:posDaDft+1:
072400160906     C                             posADft-posDaDft-1):1:3)
072500160906     C                             = 'CTE'
072600160906     C                   EVAL      PiStr=%trim(%subst(
072700160906     C                             %subst(vlrppt:posDaDft+1:
072800160906     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072900160906     C                   EXSR      CHKNUM
073000160906     C                   IF        PiInt=*on
073100160906     C                   Z-ADD     PiVal         wVABCTR_E
073200160906     C                   SETON                                        72
073300160906     C                   ENDIF
073400160906     C                   ENDIF
073500030822     C* PKB
073600030822     C                   IF        %subst(
073700030822     C                             %subst(vlrppt:posDaDft+1:
073800030822     C                             posADft-posDaDft-1):1:3)
073900030822     C                             = 'PKB'
074000030822     C                   EVAL      PiStr=%trim(%subst(
074100030822     C                             %subst(vlrppt:posDaDft+1:
074200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074300030822     C                   EXSR      CHKNUM
074400030822     C                   IF        PiNum=*on
074500030822     C                   Z-ADD     PiVal         VABPKB
074600030822     C                   ENDIF
074700030822     C                   ENDIF
074800030822     C* VLB
074900030822     C                   IF        %subst(
075000030822     C                             %subst(vlrppt:posDaDft+1:
075100030822     C                             posADft-posDaDft-1):1:3)
075200030822     C                             = 'VLB'
075300030822     C                   EVAL      PiStr=%trim(%subst(
075400030822     C                             %subst(vlrppt:posDaDft+1:
075500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075600030822     C                   EXSR      CHKNUM
075700030822     C                   IF        PiNum=*on
075800030822     C                   Z-ADD     PiVal         VABVLB
075900030822     C                   ENDIF
076000030822     C                   ENDIF
076100030822     C* QFT
076200030822     C                   IF        %subst(
076300030822     C                             %subst(vlrppt:posDaDft+1:
076400030822     C                             posADft-posDaDft-1):1:3)
076500030822     C                             = 'QFT'
076600030822     C                   EVAL      PiStr=%trim(%subst(
076700030822     C                             %subst(vlrppt:posDaDft+1:
076800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076900030822     C                   EXSR      CHKNUM
077000030822     C                   IF        PiNum=*on
077100030822     C                   Z-ADD     PiVal         VABQFT
077200030822     C                   ENDIF
077300030822     C                   ENDIF
077400170316     C* ATB
077500170316     C                   IF        %subst(
077600170316     C                             %subst(vlrppt:posDaDft+1:
077700170316     C                             posADft-posDaDft-1):1:3)
077800170316     C                             = 'ATB'
077900170316     C                   EVAL      VABATB=%trim(%subst(
078000170316     C                             %subst(vlrppt:posDaDft+1:
078100170316     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078200170316     C                   ENDIF
078300030822     C* CBO
078400030822     C                   IF        %subst(
078500030822     C                             %subst(vlrppt:posDaDft+1:
078600030822     C                             posADft-posDaDft-1):1:3)
078700030822     C                             = 'CBO'
078800030822     C                   EVAL      VABCBO=%trim(%subst(
078900030822     C                             %subst(vlrppt:posDaDft+1:
079000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079100030822     C                   ENDIF
079200030822     C* TSP
079300030822     C                   IF        %subst(
079400030822     C                             %subst(vlrppt:posDaDft+1:
079500030822     C                             posADft-posDaDft-1):1:3)
079600030822     C                             = 'TSP'
079700030822     C                   EVAL      VABTSP=%trim(%subst(
079800030822     C                             %subst(vlrppt:posDaDft+1:
079900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080000030822     C                   ENDIF
080100030822     C* VAS
080200030822     C                   IF        %subst(
080300030822     C                             %subst(vlrppt:posDaDft+1:
080400030822     C                             posADft-posDaDft-1):1:3)
080500030822     C                             = 'VAS'
080600030822     C                   EVAL      VABVAS=%trim(%subst(
080700030822     C                             %subst(vlrppt:posDaDft+1:
080800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080900030822     C                   ENDIF
081000030822     C* VCA
081100030822     C                   IF        %subst(
081200030822     C                             %subst(vlrppt:posDaDft+1:
081300030822     C                             posADft-posDaDft-1):1:3)
081400030822     C                             = 'VCA'
081500030822     C                   EVAL      VABVCA=%trim(%subst(
081600030822     C                             %subst(vlrppt:posDaDft+1:
081700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081800030822     C                   ENDIF
081900030822     C* GCA
082000030822     C                   IF        %subst(
082100030822     C                             %subst(vlrppt:posDaDft+1:
082200030822     C                             posADft-posDaDft-1):1:3)
082300030822     C                             = 'GCA'
082400030822     C                   EVAL      VABGCA=%trim(%subst(
082500030822     C                             %subst(vlrppt:posDaDft+1:
082600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082700030822     C                   ENDIF
082800030822     C* CTM
082900030822     C                   IF        %subst(
083000030822     C                             %subst(vlrppt:posDaDft+1:
083100030822     C                             posADft-posDaDft-1):1:3)
083200030822     C                             = 'CTM'
083300030822     C                   EVAL      VABCTM=%trim(%subst(
083400030822     C                             %subst(vlrppt:posDaDft+1:
083500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
083600030822     C                   ENDIF
083700030822     C* FFD
083800030822     C                   IF        %subst(
083900030822     C                             %subst(vlrppt:posDaDft+1:
084000030822     C                             posADft-posDaDft-1):1:3)
084100030822     C                             = 'FFD'
084200030822     C                   EVAL      VABFFD=%trim(%subst(
084300030822     C                             %subst(vlrppt:posDaDft+1:
084400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
084500030822     C                   ENDIF
084600030822     C* VAD
084700030822     C                   IF        %subst(
084800030822     C                             %subst(vlrppt:posDaDft+1:
084900030822     C                             posADft-posDaDft-1):1:3)
085000030822     C                             = 'VAD'
085100030822     C                   EVAL      VABVAD=%trim(%subst(
085200030822     C                             %subst(vlrppt:posDaDft+1:
085300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
085400030822     C                   ENDIF
085500030822     C* GMA
085600030822     C                   IF        %subst(
085700030822     C                             %subst(vlrppt:posDaDft+1:
085800030822     C                             posADft-posDaDft-1):1:3)
085900030822     C                             = 'GMA'
086000030822     C                   EVAL      VABGMA=%trim(%subst(
086100030822     C                             %subst(vlrppt:posDaDft+1:
086200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
086300030822     C                   ENDIF
086400030822     C* GGA
086500030822     C                   IF        %subst(
086600030822     C                             %subst(vlrppt:posDaDft+1:
086700030822     C                             posADft-posDaDft-1):1:3)
086800030822     C                             = 'GGA'
086900030822     C                   EVAL      VABGGA=%trim(%subst(
087000030822     C                             %subst(vlrppt:posDaDft+1:
087100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
087200030822     C                   ENDIF
087300030822     C* GVA
087400030822     C                   IF        %subst(
087500030822     C                             %subst(vlrppt:posDaDft+1:
087600030822     C                             posADft-posDaDft-1):1:3)
087700030822     C                             = 'GVA'
087800030822     C                   EVAL      VABGVA=%trim(%subst(
087900030822     C                             %subst(vlrppt:posDaDft+1:
088000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
088100030822     C                   ENDIF
088200030822     C* TC1
088300030822     C                   IF        %subst(
088400030822     C                             %subst(vlrppt:posDaDft+1:
088500030822     C                             posADft-posDaDft-1):1:3)
088600030822     C                             = 'TC1'
088700030822     C                   EVAL      VABTC1=%trim(%subst(
088800030822     C                             %subst(vlrppt:posDaDft+1:
088900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
089000030822     C                   ENDIF
089100030822     C* TC2
089200030822     C                   IF        %subst(
089300030822     C                             %subst(vlrppt:posDaDft+1:
089400030822     C                             posADft-posDaDft-1):1:3)
089500030822     C                             = 'TC2'
089600030822     C                   EVAL      VABTC2=%trim(%subst(
089700030822     C                             %subst(vlrppt:posDaDft+1:
089800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
089900030822     C                   ENDIF
090000030822     C* ...
090100030822     C                   ENDIF
090200030822     C                   ENDDO
090300020204     C*
090400010330     C                   ENDSR
090500010607     C*----------------------------------------------------*
090600021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
090700010607     C*----------------------------------------------------*
090800010607     C     IMPVAB        BEGSR
090900010607     C*
091000010607     C                   SETOFF                                       3132
091100030515     C*
091200010607     C                   MOVEL     datcor        VABAAS
091300010607     C                   MOVE      datcor        VABMGS
091400160902     C*
091500160902     C* Dopo aver splittato nei vari campi eseguo 1 prima "normalizzazione" x eliminare
091600160902     C* i caratteri di delimitazione testo e gli apici
091700160902     C                   EVAL      SkSplitCSV = SkSplitCSV_5
091800160902     C                   EVAL      i = 1
091900160902     C                   DOW       i < %elem(SkSplitCSV)
092000160902     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
092100160902     C     '''':' '      XLATE     SkSplitCSV(i) SkSplitCSV(i)
092200160902     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
092300160902     C                   EVAL      i = i + 1
092400160902     C                   ENDDO
092500010607     C*
092600030715     C**********
092700160902     C* Valorizzo la lista delle INTESTAZIONI colonne cliente
092800030715     C**********
092900030715     C                   IF        wGiro = *zeros
093000160902     C*
093100160902     C                   EVAL      wGiro = 1
093200160902     C                   EVAL      SkSplitFLD = SkSplitCSV
093300160902     C*
093400160902     C                   ELSE
093500160902     C*
093600160902     C**********
093700160902     C* Elaboro le righe di DATI cliente
093800160902     C**********
093900160902     C                   EVAL      wGiro = 2
094000030715     C*
094100030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
094200030715     C                   EVAL      i = 1
094300030715     C                   DOW       i < %elem(SkSplitFLD)
094400030715     C*
094500030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
094600030715     C                   IF        SkSplitFLD(i) <> *blanks AND
094700030715     C                             SkSplitCSV(i) <> *blanks
094800130624     C*** ATB
094900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
095000150420     C                   EVAL      VABATB = SkSplitCSV(i)
095100030721     C                   ENDIF
095200030715     C*** RSD
095300160902     C                   IF        %trim(SkSplitFLD(i)) =
095400160908     C                             'recipient-name'
095500030715     C                   EVAL      VABRSD = SkSplitCSV(i)
095600030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
095700030715     C     '@':'A'       XLATE     VABRSD        VABRSD
095800030715     C* ==
095900030715     C                   ENDIF
096000030721     C*** RD2
096100160902     C                   IF        %trim(SkSplitFLD(i)) =
096200160908     C                             'ship-address-2'
096300160902     C                   EVAL      VABRD2 = SkSplitCSV(i)
096400160902     C                   ENDIF
096500030715     C*** IND
096600160902     C                   IF        %trim(SkSplitFLD(i)) =
096700160908     C                             'ship-address-1'
096800030715     C                   EVAL      VABIND = SkSplitCSV(i)
096900030715     C                   ENDIF
097000030715     C*** LOD
097100160902     C                   IF        %trim(SkSplitFLD(i)) =
097200160908     C                             'ship-city'
097300030715     C                   EVAL      VABLOD = SkSplitCSV(i)
097400030715     C                   ENDIF
097500030715     C*** PRD
097600160902     C                   IF        %trim(SkSplitFLD(i)) =
097700160908     C                             'VABPRD'
097800030715     C                   EVAL      VABPRD = SkSplitCSV(i)
097900030715     C                   ENDIF
098000160902     C*** CAD
098100160902     C                   IF        %trim(SkSplitFLD(i)) =
098200160908     C                             'ship-postal-code'
098300160902     C                   EVAL      VABCAD = SkSplitCSV(i)
098400160902     C                   ENDIF
098500030721     C*** NZD
098600160902     C                   IF        %trim(SkSplitFLD(i)) =
098700160908     C                             'ship-country'
098800170315     C                   EVAL      VABNZD = SkSplitCSV(i)
098900170316     C                   Z-ADD     1             jNAZ
099000170316     C     VABNZD        LOOKUP    skNAZISO(jNAZ)                         13
099100170316     C                   IF        %found
099200170316     C                   EVAL      VABNZD = skNAZBAR(jNAZ)
099300170316     C                   ENDIF
099400030721     C                   ENDIF
099500170522     C*** RMA - 2
099600160908     C                   IF        %trim(SkSplitFLD(i)) = 'order-id'
099700170522     C                   EVAL      wVABRMA_2 = SkSplitCSV(i)
099800160908     C                   EVAL      wVABNOT = SkSplitCSV(i)
099900150515     C                   ENDIF
100000030715     C*** NOT
100100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
100200030715     C                   EVAL      VABNOT = SkSplitCSV(i)
100300030715     C                   ENDIF
100400030715     C*** NT2
100500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
100600030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
100700030715     C                   ENDIF
100800030715     C*** VCA
100900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
101000030715     C                   EVAL      VABVCA = SkSplitCSV(i)
101100030715     C                   ENDIF
101200030715     C*** VAS
101300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
101400030715     C                   EVAL      VABVAS = SkSplitCSV(i)
101500030715     C                   ENDIF
101600030715     C*** FFD
101700160902     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
101800030715     C                   EVAL      VABFFD = SkSplitCSV(i)
101900030715     C                   ENDIF
102000160902     C*** CBO
102100160902     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
102200160902     C                   EVAL      VABCBO = SkSplitCSV(i)
102300150420     C                   ENDIF
102400030721     C*** GC1
102500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
102600150420     C                   IF        %len(%trim(SkSplitCSV(i))) = 1
102700150420     C                   MOVEL     *blanks       wGC               1
102800150420     C                   EVAL      wGC = %trim(SkSplitCSV(i))
102900150420     C                   IF        SkSplitCSV(i) = 'M' OR
103000150420     C                             SkSplitCSV(i) = 'P'
103100150420     C                   MOVE(P)   wGC           VABGC1
103200150420     C                   ELSE
103300150420     C                   MOVEL(P)  wGC           VABGC1
103400150420     C                   ENDIF
103500150420     C                   ENDIF
103600030721     C                   ENDIF
103700030721     C*** GC2
103800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
103900120314     C                   IF        %len(%trim(SkSplitCSV(i))) = 1
104000120314     C                   MOVEL     *blanks       wGC               1
104100120314     C                   EVAL      wGC = %trim(SkSplitCSV(i))
104200120314     C                   IF        SkSplitCSV(i) = 'M' OR
104300120314     C                             SkSplitCSV(i) = 'P'
104400120314     C                   MOVE(P)   wGC           VABGC2
104500120314     C                   ELSE
104600120314     C                   MOVEL(P)  wGC           VABGC2
104700120314     C                   ENDIF
104800120314     C                   ENDIF
104900030721     C                   ENDIF
105000030721     C*** TSP
105100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
105200030721     C                   EVAL      VABTSP = SkSplitCSV(i)
105300030721     C                   ENDIF
105400030721     C*** NAS
105500160902     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
105600030721     C                   EVAL      VABNAS = SkSplitCSV(i)
105700030721     C                   ENDIF
105800030721     C*** TIC
105900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
106000030721     C                   EVAL      VABTIC = SkSplitCSV(i)
106100030721     C                   ENDIF
106200030721     C*** GCA
106300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
106400030721     C                   EVAL      VABGCA = SkSplitCSV(i)
106500030721     C                   ENDIF
106600030721     C*** XCO
106700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
106800030721     C                   EVAL      VABXCO = SkSplitCSV(i)
106900030721     C                   ENDIF
107000030721     C*** CTM
107100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
107200030721     C                   EVAL      VABCTM = SkSplitCSV(i)
107300030721     C                   ENDIF
107400030721     C*** TCR
107500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
107600030721     C                   EVAL      VABTCR = SkSplitCSV(i)
107700030721     C                   ENDIF
107800030721     C*** CTS
107900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
108000030721     C                   EVAL      VABCTS = SkSplitCSV(i)
108100030721     C                   ENDIF
108200030721     C*** FTM
108300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
108400030721     C                   EVAL      VABFTM = SkSplitCSV(i)
108500030721     C                   ENDIF
108600030721     C*** VAD
108700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
108800030721     C                   EVAL      VABVAD = SkSplitCSV(i)
108900030721     C                   ENDIF
109000030721     C*** GMA
109100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
109200030721     C                   EVAL      VABGMA = SkSplitCSV(i)
109300030721     C                   ENDIF
109400030721     C*** GGA
109500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
109600030721     C                   EVAL      VABGGA = SkSplitCSV(i)
109700030721     C                   ENDIF
109800030721     C*** GVA
109900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
110000030721     C                   EVAL      VABGVA = SkSplitCSV(i)
110100030721     C                   ENDIF
110200030721     C*** TC1
110300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
110400030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
110500030721     C                   ENDIF
110600030721     C*** TC2
110700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
110800030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
110900030721     C                   ENDIF
111000030721     C*** SCL
111100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
111200030721     C                   EVAL      VABSCL = SkSplitCSV(i)
111300030721     C                   ENDIF
111400030721     C*** RMO
111500160902     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
111600030721     C                   EVAL      VABRMO = SkSplitCSV(i)
111700030721     C                   ENDIF
111800160902     C*** CMO
111900160902     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
112000160902     C                   EVAL      VABCMO = SkSplitCSV(i)
112100160902     C                   ENDIF
112200030721     C*** NMO
112300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
112400030721     C                   EVAL      VABNMO = SkSplitCSV(i)
112500030721     C                   ENDIF
112600160908     C*** NOT_A
112700160908     C                   IF        %trim(SkSplitFLD(i)) =
112800160908     C                             'buyer-name'
112900160908     C                   EVAL      wVATNOT_A = SkSplitCSV(i)
113000160908     C                   ENDIF
113100160902     C*** NOT_B
113200160902     C                   IF        %trim(SkSplitFLD(i)) =
113300160908     C                             'buyer-phone-number'
113400160902     C                   EVAL      wVATNOT_B = SkSplitCSV(i)
113500160902     C                   ENDIF
113600150420     C*** NOT_I
113700160902     C                   IF        %trim(SkSplitFLD(i)) =
113800160908     C                             'buyer-email'
113900150420     C                   EVAL      wVATNOT_I = SkSplitCSV(i)
114000150420     C                   ENDIF
114100150420     C*** NOT_S
114200170315     C                   IF        %trim(SkSplitFLD(i)) =
114300170315     C                             'ship-phone-number'
114400150420     C                   EVAL      wVATNOT_S = SkSplitCSV(i)
114500150420     C                   ENDIF
114600030721     C*
114700070705     C* Reperisco quindi i campi numerici...
114800030721     C*** RMN
114900160908     C                   IF        %trim(SkSplitFLD(i)) =
115000160908     C                             'order-item-id'
115100170522     C                   EVAL      wVABRMA_1 = SkSplitCSV(i)
115200030721     C                   EVAL      PiStr=SkSplitCSV(i)
115300030721     C                   EXSR      CHKNUM
115400030721     C                   IF        PiInt=*on
115500030721     C                   Z-ADD     PiVal         VABRMN
115600030721     C                   ELSE
115700030721     C                   SETON                                        32
115800030721     C                   EVAL      VABRMN = 1
115900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116000030721     C                             + ' ' + 'VABRMN'
116100030721     C                   ENDIF
116200030721     C                   ENDIF
116300030721     C*** NSP
116400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
116500030721     C                   EVAL      PiStr=SkSplitCSV(i)
116600030721     C                   EXSR      CHKNUM
116700100517     C                   IF        PiInt=*on
116800030721     C                   Z-ADD     PiVal         VABNSP
116900030721     C                   ELSE
117000030721     C                   SETON                                        32
117100030721     C                   EVAL      VABNSP = *zeros
117200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117300030721     C                             + ' ' + 'VABNSP'
117400030721     C                   ENDIF
117500030721     C                   ENDIF
117600160902     C*** NCL
117700160902     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
117800030721     C                   EVAL      PiStr=SkSplitCSV(i)
117900030721     C                   EXSR      CHKNUM
118000030721     C                   IF        PiInt=*on
118100030721     C                   Z-ADD     PiVal         VABNCL
118200030721     C                   ELSE
118300030721     C                   SETON                                        32
118400030721     C                   EVAL      VABNCL = *zeros
118500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118600030721     C                             + ' ' + 'VABNCL'
118700030721     C                   ENDIF
118800030721     C                   ENDIF
118900030721     C*** PKB
119000160902     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
119100030721     C                   EVAL      PiStr=SkSplitCSV(i)
119200030721     C                   EXSR      CHKNUM
119300030721     C                   IF        PiNum=*on
119400030721     C                   Z-ADD     PiVal         VABPKB
119500030721     C                   ELSE
119600030721     C                   SETON                                        32
119700030721     C                   EVAL      VABPKB = *zeros
119800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119900030721     C                             + ' ' + 'VABPKB'
120000030721     C                   ENDIF
120100030721     C                   ENDIF
120200160908     C*** CAS
120300160908     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
120400160908     C                   IF        SkSplitCSV(i) <> *blanks
120500160908     C                   EVAL      wFlgCAS = 1
120600160908     C                   ENDIF
120700160902     C                   EVAL      PiStr=SkSplitCSV(i)
120800160902     C                   EXSR      CHKNUM
120900160902     C                   IF        PiNum=*on
121000160902     C                   Z-ADD     PiVal         VABCAS
121100160902     C                   MOVEL     'EUR'         VABVCA
121200160902     C                   ELSE
121300160902     C                   SETON                                        32
121400160902     C                   EVAL      VABCAS = *zeros
121500160902     C                   EVAL      vinmsg = %trimr(vinmsg)
121600160902     C                             + ' ' + 'VABCAS'
121700160902     C                   ENDIF
121800160902     C                   ENDIF
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
151900170316     C                   IF        VABNZD = *blanks AND
152000170316     C                             VABPRD = *blanks
152100160908     C                   CLEAR                   TISI95DS
152200160908     C                   EVAL      I95TCN = '3'
152300160908     C                   Z-ADD     datcor        I95DAT
152400160908     C                   EVAL      I95NAR = VABNZD
152500160908     C                   EVAL      I95CAP = VABCAD
152600160908     C                   EVAL      I95LOC = VABLOD
152700160908     C                   CALL      'TISI95R'
152800160908     C                   PARM                    TISI95DS
152900160908     C                   EVAL      VABPRD = O95PRV
153000160908     C                   ENDIF
153100160908     C*
153200160908     C* Eseguo routine finale x considerazioni specifiche su importi/divise
153300160908     C                   EXSR      CHKIMPDIV
153400160908     C*
153500160908     C                   ENDSR
153600160908     C***
153700010601
153800020204
153900020204     C*----------------------------------------------------*
154000020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
154100020204     C*----------------------------------------------------*
154200020204     C     CHKIMPDIV     BEGSR
154300060524     C*
154400060524     C* Verifico il codice bolla in base al tipo porto e alla presenza o meno del contrassegno
154500160906     C                   IF        vabCAS  > *zeros OR
154600160906     C                             wFlgCAS > *zeros
154700060524     C                   IF        vabCBO = '1'
154800060524     C                   EVAL      vabCBO = '4'
154900060524     C                   ENDIF
155000060524     C                   IF        vabCBO = '2'
155100060524     C                   EVAL      vabCBO = '6'
155200060524     C                   ENDIF
155300060524     C                   ENDIF
155400020204     C*
155500020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
155600020204     C                   Z-ADD     *zeros        wrkDec            9 9
155700020204     C*
155800020204     C* Come prima cosa effettuo considerazioni sulla divisa
155900020204     C                   IF        vabIAS > *zeros
156000150420     C                   IF        vabVAS =  *blanks
156100150420     C                   EVAL      vabVAS =  'EUR'
156200020204     C                   ENDIF
156300020204     C                   ENDIF
156400020204     C*
156500020204     C                   IF        vabCAS > *zeros
156600150420     C                   IF        vabVCA <> *blanks
156700150420     C                   EVAL      vabVCA =  'EUR'
156800020204     C                   ENDIF
156900020204     C                   ENDIF
157000020204     C*
157100020204     C                   IF        vabVMD > *zeros
157200150420     C                   IF        vabVAD <> *blanks
157300150420     C                   EVAL      vabVAD =  'EUR'
157400020204     C                   ENDIF
157500020204     C                   ENDIF
157600020204     C*
157700020204     C                   ENDSR
157800020204     C***
157900010330
158000010330
158100010330     C*----------------------------------------------------*
158200010330     C*  CONTROLLO NUMERICITA' CAMPI
158300010330     C*----------------------------------------------------*
158400010330     C     CHKNUM        BEGSR
158500010330     C*
158600010606     C                   IF        PiDecChr = *blanks
158700030715     C                   EVAL      PiDecChr = CharNUM
158800010606     C                   ENDIF
158900010606     C*
159000010606     C                   CALL(e)   'ISNUMERIC'
159100010330     C                   PARM                    PiStr            30
159200010606     C                   PARM                    PiDecChr          1
159300010330     C                   PARM      *ZEROS        PiVal            30 9
159400010330     C                   PARM      '0'           PiInt             1
159500010330     C                   PARM      '0'           PiNum             1
159600010330     C                   IF        %error
159700010606     C                   EVAL      PiNum=*off
159800010330     C                   ENDIF
159900010330     C*
160000010330     C                   ENDSR
160100010330     C***
160200160906
160300160906
160400160906      /TITLE Valorizzazione Progressivo Applicazione
160500160906     C     calprog       begsr
160600160906     C*
160700160906     C                   movel     *blanks       dwlisv            2
160800160906     C                   movel     *all'0'       dwlprg           10
160900160906     C                   z-add     *zeros        wrkprg            8 0
161000160906     C*
161100160906     C                   eval      dwlisv = vlrisv
161200160906     C*
161300170324     C     *start        setll     tis7prgf
161400160906     C                   read(e)   tis7prgf
161500160906     C                   if        not %error
161600160906     C                   eval      dwlprg = f_tis7prgf
161700160906     C*
161800160906     C                   move(p)   dwlprg        wrkprg
161900160906     C                   add       1             wrkprg
162000160906     C                   move(p)   wrkprg        dwlprg
162100160906     C                   movel     dwlisv        dwlprg
162200160906     C*
162300160906     C                   eval      f_tis7prgf = dwlprg
162400160906     C                   update    tis7prg0
162500170324     C                   else
162600170324     C                   eval      f_tis7prgf = 'XX00000000'
162700160906     C                   endif
162800160906     C*
162900160906     C                   endsr
163000050421
163100160908
163200160908     C     *pssr         BEGSR
163300160908     C*
163400160908     C                   if        %open(tivin00r)
163500160908     C                   close     tivin00r
163600160908     C                   endif
163700160908     C                   if        %open(fivabwwr)
163800160908     C                   close     fivabwwr
163900160908     C                   endif
164000160908     C                   if        %open(fivatwwr)
164100160908     C                   close     fivatwwr
164200160908     C                   endif
164300160908     C*
164400160908     C* Effettuo la chiamata al CLLE preposto
164500160908     C  N50              call(e)   'TITVVTC'
164600160908     C                   parm                    parccm
164700160908     C                   parm                    parmbr
164800160908     C                   parm      '2'           paropz
164900160908     C*
165000160908     C                   eval      wrkesito = '2'
165100160908     C*
165200160908     C                   seton                                        LR
165300160908     C*
165400160908     C                   ENDSR     '*CANCL'
165500160908     C***
165600100419
165700100419
165800160902     C     normCSV       BEGSR
165900160902     C*
166000160902     C                   z-add     *zeros        wLenVINDTA        4 0
166100160902     C                   z-add     *zeros        wFlgVINDTA        1 0
166200160902     C                   setoff                                       80
166300160902     C*
166400160902     C                   dow       wLenVINDTA < %len(vindta)
166500160902     C                   eval      wLenVINDTA = wLenVINDTA + 1
166600160902     C                   movel     *blanks       wAlfa1            1
166700160902     C                   eval      wAlfa1 = %subst(vindta:wLenVINDTA:1)
166800160902     C*
166900160902     C* Se byte inferiore a HEX40 è un byte "non stampabile" per cui considero *blank
167000170315     C                   if        wAlfa1 <  X'40' AND
167100170315     C                             wAlfa1 <> CharCSV
167200160902     C                   eval      %subst(vindta:wLenVINDTA:1) = *blanks
167300160902     C                   endif
167400160902     C*
167500160902     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
167600160902     C                   if        wAlfa1 <> CharCSV and
167700160902     C                             wAlfa1 <> CharTXT and
167800160902     C                             wAlfa1 <> CharNUM
167900160902     C                   z-add     1             wFlgVINDTA
168000160902     C                   endif
168100160902     C*
168200160902     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
168300160902     C* separatori campi
168400160902     C                   if        CharTXT <> *blanks
168500160902     C                   if        wAlfa1 = CharTXT
168600160902     C                   if        not *in80
168700160902     C                   seton                                        80
168800160902     C                   else
168900160902     C                   setoff                                       80
169000160902     C                   endif
169100160902     C                   endif
169200160902     C                   if        wAlfa1 = CharCSV and *in80 = *on
169300160902     C                   eval      %subst(vindta:wLenVINDTA:1) = CharSOS
169400160902     C                   endif
169500160902     C                   endif
169600160902     C*
169700160902     C                   enddo
169800100419     C*
169900100419     C                   ENDSR
170000100419     C***
170100150420
170200150420
170300150420      /TITLE Scrittura record in file TIVGD00F (file VAS generico download)
170400160902     C     wriVAB_VGD    BEGSR
170500160909     C*
170600160909     C* Verifica campi "particolari" VAB
170700160909     C                   exsr      repNSP
170800160909     C*
170900160909     C                   setoff                                       45
171000160909     C                   if        depSPE  = *zeros OR
171100160909     C                             depSPE <> vabNSP
171200160909     C                   seton                                        45
171300160909     C                   eval      depSPE  = vabNSP
171400160909     C                   endif
171500160909     C*
171600160909     C                   if        not *in31
171700150420     C*
171800160902     C* Se primo giro allora scrivo testata
171900160902     C                   if        wVGD_Head = *zeros
172000160902     C                   eval      wVGD_Head = 1
172100160902     C                   eval      wBufferOut =
172200170316     C                             %trim(pptCHR)+'VABATB' +%trim(pptCHR)+pptCSV+
172300160902     C                             %trim(pptCHR)+'VABCCM' +%trim(pptCHR)+pptCSV+
172400160902     C                             %trim(pptCHR)+'VABLNP' +%trim(pptCHR)+pptCSV+
172500160902     C                             %trim(pptCHR)+'VABCTR' +%trim(pptCHR)+pptCSV+
172600160902     C                             %trim(pptCHR)+'VABRSD' +%trim(pptCHR)+pptCSV+
172700160902     C                             %trim(pptCHR)+'VABRD2' +%trim(pptCHR)+pptCSV+
172800160902     C                             %trim(pptCHR)+'VABIND' +%trim(pptCHR)+pptCSV+
172900160902     C                             %trim(pptCHR)+'VABLOD' +%trim(pptCHR)+pptCSV+
173000160902     C                             %trim(pptCHR)+'VABCAD' +%trim(pptCHR)+pptCSV+
173100160902     C                             %trim(pptCHR)+'VABPRD' +%trim(pptCHR)+pptCSV+
173200160902     C                             %trim(pptCHR)+'VABCBO' +%trim(pptCHR)+pptCSV+
173300160902     C                             %trim(pptCHR)+'VABRMO' +%trim(pptCHR)+pptCSV+
173400160902     C                             %trim(pptCHR)+'VABRMN' +%trim(pptCHR)+pptCSV+
173500160902     C                             %trim(pptCHR)+'VABRMA' +%trim(pptCHR)+pptCSV+
173600160902     C                             %trim(pptCHR)+'VABNAS' +%trim(pptCHR)+pptCSV+
173700160902     C                             %trim(pptCHR)+'VABNCL' +%trim(pptCHR)+pptCSV+
173800160902     C                             %trim(pptCHR)+'VABPKB' +%trim(pptCHR)+pptCSV+
173900160902     C                             %trim(pptCHR)+'VABCAS' +%trim(pptCHR)+pptCSV+
174000160902     C                             %trim(pptCHR)+'VABVCA' +%trim(pptCHR)+pptCSV+
174100160902     C                             %trim(pptCHR)+'VABFFD' +%trim(pptCHR)+pptCSV+
174200160902     C                             %trim(pptCHR)+'VABIAS' +%trim(pptCHR)+pptCSV+
174300160902     C                             %trim(pptCHR)+'VABVAS' +%trim(pptCHR)+pptCSV+
174400160902     C                             %trim(pptCHR)+'VABNOT' +%trim(pptCHR)+pptCSV+
174500170510     C                             %trim(pptCHR)+'VABTSP' +%trim(pptCHR)+pptCSV+
174600160902     C                             %trim(pptCHR)+'VABNRC' +%trim(pptCHR)+pptCSV+
174700160902     C                             %trim(pptCHR)+'VABTRC' +%trim(pptCHR)+pptCSV+
174800160902     C                             %trim(pptCHR)+'VABEMD' +%trim(pptCHR)+pptCSV+
174900160902     C                             %trim(pptCHR)+'VABCEL' +%trim(pptCHR)
175000160902     C                   exsr      wriVGD
175100160902     C                   endif
175200160902     C*
175300160902     C* Quindi scrivo dati spedizione
175400160909     C   45              eval      wBufferOut =
175500170316     C                             %trim(pptCHR)+%trim(VABATB)
175600170316     C                                                    +%trim(pptCHR)+pptCSV+
175700150420     C                             %trim(pptCHR)+%trim(%editc(VABCCM:'X'))
175800150420     C                                                    +%trim(pptCHR)+pptCSV+
175900150420     C                             %trim(pptCHR)+%trim(%editc(VABLNP:'X'))
176000150420     C                                                    +%trim(pptCHR)+pptCSV+
176100150420     C                             %trim(pptCHR)+%trim(%editc(VABCTR:'X'))
176200150420     C                                                    +%trim(pptCHR)+pptCSV+
176300150420     C                             %trim(pptCHR)+%trim(VABRSD)
176400150420     C                                                    +%trim(pptCHR)+pptCSV+
176500160902     C                             %trim(pptCHR)+%trim(VABRD2)
176600160902     C                                                    +%trim(pptCHR)+pptCSV+
176700150420     C                             %trim(pptCHR)+%trim(VABIND)
176800150420     C                                                    +%trim(pptCHR)+pptCSV+
176900150420     C                             %trim(pptCHR)+%trim(VABLOD)
177000150420     C                                                    +%trim(pptCHR)+pptCSV+
177100150420     C                             %trim(pptCHR)+%trim(VABCAD)
177200150420     C                                                    +%trim(pptCHR)+pptCSV+
177300150420     C                             %trim(pptCHR)+%trim(VABPRD)
177400150420     C                                                    +%trim(pptCHR)+pptCSV+
177500150420     C                             %trim(pptCHR)+%trim(VABCBO)
177600150420     C                                                    +%trim(pptCHR)+pptCSV+
177700150420     C                             %trim(pptCHR)+%trim(VABRMO)
177800150420     C                                                    +%trim(pptCHR)+pptCSV+
177900150420     C                             %trim(pptCHR)+%trim(%editc(VABRMN:'4'))
178000150420     C                                                    +%trim(pptCHR)+pptCSV+
178100150420     C                             %trim(pptCHR)+%trim(VABRMA)
178200150420     C                                                    +%trim(pptCHR)+pptCSV+
178300150420     C                             %trim(pptCHR)+%trim(VABNAS)
178400150420     C                                                    +%trim(pptCHR)+pptCSV+
178500150420     C                             %trim(pptCHR)+%trim(%editc(VABNCL:'4'))
178600150420     C                                                    +%trim(pptCHR)+pptCSV+
178700150420     C                             %trim(pptCHR)+%trim(%editc(VABPKB:'4'))
178800150420     C                                                    +%trim(pptCHR)+pptCSV+
178900150420     C                             %trim(pptCHR)+%trim(%editc(VABCAS:'4'))
179000150420     C                                                    +%trim(pptCHR)+pptCSV+
179100150420     C                             %trim(pptCHR)+%trim(VABVCA)
179200150420     C                                                    +%trim(pptCHR)+pptCSV+
179300150420     C                             %trim(pptCHR)+%trim(VABFFD)
179400150420     C                                                    +%trim(pptCHR)+pptCSV+
179500150420     C                             %trim(pptCHR)+%trim(%editc(VABIAS:'4'))
179600150420     C                                                    +%trim(pptCHR)+pptCSV+
179700150420     C                             %trim(pptCHR)+%trim(VABVAS)
179800150420     C                                                    +%trim(pptCHR)+pptCSV+
179900150421     C                             %trim(pptCHR)+%trim(VABNOT)
180000150421     C                                                    +%trim(pptCHR)+pptCSV+
180100170510     C                             %trim(pptCHR)+%trim(VABTSP)
180200170510     C                                                    +%trim(pptCHR)+pptCSV+
180300160902     C                             %trim(pptCHR)+%trim(wVATNOT_A)
180400160902     C                                                    +%trim(pptCHR)+pptCSV+
180500160902     C                             %trim(pptCHR)+%trim(wVATNOT_B)
180600160902     C                                                    +%trim(pptCHR)+pptCSV+
180700150420     C                             %trim(pptCHR)+%trim(wVATNOT_I)
180800150420     C                                                    +%trim(pptCHR)+pptCSV+
180900150420     C                             %trim(pptCHR)+%trim(wVATNOT_S)
181000150420     C                                                    +%trim(pptCHR)
181100160909     C   45              exsr      wriVGD
181200150420     C*
181300160909     C                   endif
181400160909     C*
181500150420     C                   ENDSR
181600150420     C*------------------------------------------------------------------------*
181700150420
181800150420
181900150420
182000150420      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
182100150420     C     wriVGD        BEGSR
182200150420     C*
182300150420     C                   clear                   tivgd000
182400150420     C                   eval      vgdDTA = wBufferOut
182500150420     C                   eval      vgdTIP = 'EB'
182600150420     C                   eval      vgdKSU = vlrKSC
182700150420     C                   eval      vgdTSC = 'WW'
182800150420     C                   eval      vgdDAT = datcor
182900160906     C                   eval      vgdPRG = f_tis7prgf
183000160920     C                   eval      vgdPGM = procname
183100160906     C                   eval      vgdSTO = '?'
183200150420     C                   write     tivgd000
183300150420     C*
183400150420     C                   ENDSR
183500150420     C*------------------------------------------------------------------------*
183600160902
183700160902
183800160902     C*----------------------------------------------------*
183900160902     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
184000160902     C*----------------------------------------------------*
184100160902     C     SR_SPLIT      BEGSR
184200160902     C*
184300160902     C* Chiamo il *pgm d utilità x lo "split" della riga delle intestazioni
184400160902     C                   CALL      'XSPLIT2'
184500160902     C                   PARM                    InStringa
184600160902     C                   PARM                    InSepara
184700160902     C                   PARM      '5'           InTypeOut
184800160902     C                   PARM      ''            wSkParam
184900160902     C                   PARM                    OutErrore
185000160902     C                   PARM                    OutMsg
185100160902     C                   MOVEA     wSkParam      SkSplitCSV_5
185200160902     C*
185300160902     C                   ENDSR
185400170316
185500170316
185600170316
185700170316     C*--------------------------------------------------------
185800170316     C* CARTAB - CARICAMENTO DATI TABELLARI                   *
185900170316     C*--------------------------------------------------------
186000170316     C     CARTAB        BEGSR
186100170316     C*
186200170316     C* TABELLA '15' - NAZIONI
186300170316     C                   clear                   skNAZISO
186400170316     C                   clear                   skNAZBAR
186500170316     C                   eval      tblKUT = 1
186600170316     C                   eval      tblCOD = '15'
186700170316     C     KEYtabP       setll     tabel00f
186800170316     C     KEYtabP       reade     tabel00f
186900170316     C                   dow       not %eof(tabel00f)
187000170316     C                   if        tblFLG = *blanks
187100170316     C                   movel(p)  tblUNI        ds15
187200170316     C                   add       1             jNAZ
187300170316     C                   eval      skNAZBAR(jNAZ) = tblKEY
187400170316     C                   eval      skNAZISO(jNAZ) = §15ISO
187500170316     C                   endif
187600170316     C     KEYtabP       reade     tabel00f
187700170316     C                   enddo
187800170316     C*
187900170316     C                   ENDSR
188000170316     C***
188100170316
188200150420
188300150420
188400000613     C     *inzsr        BEGSR
188500990910     C*
188600990910     C     *entry        plist
188700990920     C                   parm                    tivlrds
188800990921     C                   parm      wrkesito      esito
188900000724     C                   parm                    prmlit
189000000710     C                   parm                    prmfir
189100010330     C*
189200010330     C* CALCOLA LA DATA CORRENTE
189300150420     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
189400150420     C                   eval      datcor = %dec(%date() : *ISO)
189500170316     C*
189600170316     C* Chiave su TABEL00F - parziale
189700170316     C     KEYtabP       KLIST
189800170316     C                   KFLD                    tblKUT
189900170316     C                   KFLD                    tblCOD
190000000613     C*
190100000613     C                   ENDSR
190200000613     C***
