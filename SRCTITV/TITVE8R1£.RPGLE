000100060504      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR (x SCHENKER Cod. 2493133)
000200101105     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600040723     FFIVABwwr  O    E             DISK    usropn
000700060504     FFIVATwwr  O    E             DISK    usropn
000800040928     Ftitve8p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000040928     Ftitve8ps  O    f  198        PRINTER usropn
001100000621     F                                     oflind(*inob)
001200000313     D*
001300000613     D*------------
001400000613     D* COMANDI
001500000613     D*------------
001600011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001700010330     D*----------------------------------------------------
001800010330     D* DICHIARAZIOINE VARIABILI DI WRK
001900010330     D*----------------------------------------------------
002000010330     D dscmz         e ds                  inz
002100010330     D psds           sds
002200010330     D  procname         *PROC
002300010330     D tivlrds       e ds                  extname(tivlr00f)
002400040607     D tisi95ds      e ds
002500010330     D esito           s              1
002600010330     D prmlit          s             10
002700010330     D prmfir          s             10
002800010330     D wrkesito        s                   like(esito)
002900010330     D wrkdata         s               d
003000010330     D wrkora          s               t
003100010330     D rrnum           s              6  0 INZ(*zeros)
003200010330     D recko           s            150    INZ(*blanks)
003300011113     D depcmd          s            150    INZ(*blanks)
003400060504     D parccm          s              8    INZ(*blanks)
003500060504     D parmbr          s             10    INZ(*blanks)
003600060504     D paropz          s              1    INZ(*blanks)
003700060504     D chkcall         s              1    INZ(*blanks)
003800100706
003900100706
004000100706     D*------------------
004100100706     D* DS REPERIMENTO NUMERATORE
004200100706     D*------------------
004300100706     D trul33ds      e ds                  inz
004400100706     D*------------------
004500100706     D* DS ARCHITETTURA
004600100706     D*------------------
004700100706     D kpjba         e ds                  inz
004800100706
004900010330
005000010330
005100010330
005200990915     C                   time                    wrkdata
005300990915     C                   time                    wrkora
005400000913     C                   reset                   rrnum
005500990921     C                   reset                   esito
005600990921     C                   reset                   wrkesito
005700010601     C*
005800010601     C                   exsr      opeini
005900010605     C                   exsr      rwvab
006000060504     C*
006100060504     C* Effettuo la chiamata al CLLE preposto
006200060504     C                   call(e)   'TITVVTC'
006300060504     C                   parm                    parccm
006400060504     C                   parm                    parmbr
006500060504     C                   parm      '2'           paropz
006600040928     C*
006700040928     C* Lancio TISI95R solo x chiusura
006800040928     C                   CLEAR                   TISI95DS
006900040928     C                   EVAL      I95TLA = 'C'
007000040928     C                   CALL      'TISI95R'
007100040928     C                   PARM                    TISI95DS
007200010601     C*
007300010601     C                   seton                                        lr
007400010601
007500010601
007600010601
007700010601
007800010601     C*--------------------------------------------------------
007900010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
008000010601     C*--------------------------------------------------------
008100010601     C     PREELA        BEGSR
008200010601     C*
008300010601     C* SE OCCORRE SPEDIRE IN FILIALE
008400010601     C                   if        invfil <> *zeros and
008500010601     C                             flgGiro = '0'
008600010601     C*
008700010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
008800010601     C                   eval      flgGiro = '1'
008900000724     C*
009000000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
009100000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
009200010601     C     invfil        chain     azorg01l
009300000724     C                   if        %found
009400000724     C                   movel(p)  CMD(1)        depcmd
009500020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
009600000724     C*
009700000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
009800011113     C                   Z-ADD     150           LENGH            15 5
009900000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
010000000724     C                   PARM                    depcmd
010100000724     C                   PARM                    LENGH
010200000724     C*
010300000724     C                   endif
010400000616     C*
010500000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
010600000616     C                   movel(p)  CMD(2)        depcmd
010700000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
010800000616     C*
010900000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
011000011113     C                   Z-ADD     150           LENGH            15 5
011100000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
011200000616     C                   PARM                    depcmd
011300000616     C                   PARM                    LENGH
011400000613     C*
011500040928     C                   if        not %open(titve8ps)
011600040928     C                   open      titve8ps
011700000616     C                   except    testdett
011800000613     C                   endif
011900010601     C*
012000010601     C                   endif
012100010601     C*
012200010601     C                   ENDSR
012300010601     C***
012400010601
012500010601
012600010601
012700010601
012800010601
012900010601
013000010601
013100010601     C*--------------------------------------------------------
013200010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
013300010601     C*--------------------------------------------------------
013400010601     C     ENDELA        BEGSR
013500990915     C*
013600000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
013700000613     C*
013800040928     C                   if        %open(titve8ps)
013900000616     C                   except    findett
014000040928     C                   close     titve8ps
014100000613     C                   endif
014200000616     C*
014300000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
014400011113     C                   Z-ADD     150           LENGH            15 5
014500010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
014600000616     C                   PARM                    CMD(3)
014700000616     C                   PARM                    LENGH
014800000616     C*
014900010601     C                   ENDSR
015000010601     C***
015100010601
015200010601
015300010601
015400010601
015500000613
015600000613     C*--------------------------------------------------------
015700000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
015800000613     C*--------------------------------------------------------
015900000621     C     STPR          BEGSR
016000000613     C*
016100040928     C                   if        not %open(titve8p)
016200040928     C                   open      titve8p
016300990915     C                   endif
016400990915     C*
016500990915     C                   except    riepilogo
016600990915     C*
016700040928     C                   if        %open(titve8p)
016800040928     C                   close     titve8p
016900990914     C                   endif
017000000613     C*
017100000613     C                   ENDSR
017200000613     C***
017300000613
017400010330
017500010601
017600010601
017700010601
017800010330     C*--------------------------------------------------------
017900040723     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
018000010330     C*--------------------------------------------------------
018100010605     C     RWVAB         BEGSR
018200010330     C*
018300010330     C                   if        not %open(tivin00r)
018400010330     C                   open      tivin00r
018500010330     C                   endif
018600040723     C                   if        not %open(fivabwwr)
018700040723     C                   open      fivabwwr
018800010330     C                   endif
018900060504     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
019000060504     C                   exsr      prevat
019100060504     C*
019200060504     C                   if        chkcall = '0'
019300060504     C*
019400060504     C                   if        not %open(fivatwwr)
019500060504     C                   open      fivatwwr
019600060504     C                   endif
019700010330     C*
019800060504     C                   clear                   §CTROK
019900060504     C                   clear                   §CTROKVT          5 0
020000010604     C                   clear                   §CTRMO
020100010604     C                   clear                   §CTRNO
020200010330     C*
020300010330     C                   DO        *HIVAL
020400010330     C*
020500010330     C                   READ      tivin00r                               70
020600010618     C*
020700010618     C* Dopo ogni lettura verifico se ci sono stati record OK
020800010618     C                   if        vinflg = '1'
020900010618     C                   eval      flgOk = '1'
021000010618     C                   endif
021100010618     C*
021200010330     C                   if        vindta > *blanks
021300010330     C                   add       1             rrnum
021400010330     C*
021500010601     C                   if        *in70 = *off and
021600010330     C                             (vinflg = *blanks
021700010330     C                              or vinflg = '0'
021800010330     C                              or vinflg = '2')
021900010330     C*
022000010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
022100010711     C                   if        vinflg = *blanks or vinflg = '0'
022200010711     C                   clear                   vinmsg
022300010711     C                   endif
022400010601     C*
022500010605     C                   exsr      impvab
022600010601     C*
022700010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
022800010605     C                   eval      depfil = VABLNP
022900010601     C                   exsr      repfil
023000010601     C                   if        depfil = invfil
023100021025     C                   if        vlrpoi = 999
023200021025     C                   MOVE(P)   invfil        VABFGS
023300021025     C                   else
023400021025     C                   MOVE(P)   vlrpoi        VABFGS
023500021025     C                   endif
023600010601     C*
023700010601     C                   exsr      PREELA
023800010601     C*
023900010604     C* Ebbene...
024000010604     C*
024100010604     C  N31              ADD       1             §CTROK            7 0
024200010604     C   32              ADD       1             §CTRMO            7 0
024300010604     C   31              ADD       1             §CTRNO            7 0
024400010604     C*
024500010604     C                   if        *in31 = *off and
024600010604     C                             *in32 = *off
024700010604     C                   eval      vinflg = '1'
024800010604     C                   else
024900010604     C                   eval      recko = vindta
025000010604     C                   except    rigadett
025100010604     C                   eval      vinflg = '2'
025200010604     C                   endif
025300010604     C                   endif
025400010601     C*
025500010604     C                   endif
025600010604     C*
025700010330     C                   else
025800010330     C                   eval      vinflg = '1'
025900010330     C                   endif
026000010601     C*
026100010601     C  N70              update    tivin000
026200010330     C*
026300010330     C  N70              ENDdo
026400100824     C*
026500100824     C* Scarico il buffer del FIVAB rimasto eventualmente "in canna"
026600100824     C                   EXSR      WRIVAB
026700060504     C*
026800060504     C                   endif                                                  * chkcall
026900010601     C*
027000010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
027100010601     C                   if        cntNonEl = *zeros or
027200010601     C                             flgMulti = '0'
027300010330     C* Se non ci sono record con errori ...
027400010601     C                   if        §ctrno = 0 and
027500010604     C                             §ctrmo = 0 and
027600010601     C                             flgStato <> '2'
027700010330     C* ... restituisco esito OK.
027800010330     C                   eval      wrkesito = '0'
027900010330     C                   else
028000010330     C                   if        §ctrok > 0
028100010330     C                   eval      wrkesito = '1'
028200010330     C                   else
028300010615     C                   if        flgOk = '0'
028400010615     C                   eval      wrkesito = '2'
028500010615     C                   else
028600010615     C                   eval      wrkesito = '6'
028700010615     C                   endif
028800010330     C                   endif
028900010330     C                   endif
029000010601     C                   else
029100010601     C                   eval      wrkesito = '9'
029200010601     C                   endif
029300010330     C*
029400010330     C                   if        %open(tivin00r)
029500010330     C                   close     tivin00r
029600010330     C                   endif
029700040723     C                   if        %open(fivabwwr)
029800040723     C                   close     fivabwwr
029900010330     C                   endif
030000060504     C                   if        %open(fivatwwr)
030100060504     C                   close     fivatwwr
030200060504     C                   endif
030300010601     C*
030400010601     C                   if        vlrpoi <> 999
030500010601     C                   eval      invfil = vlrpoi
030600010601     C                   endif
030700010330     C*
030800010330     C                   if        §ctrok > 0
030900010601     C                             and invfil > *zeros
031000010330     C                   exsr      invio
031100010330     C                   endif
031200010601     C*
031300010618     C                   if        flgGiro = '1'
031400010601     C                   exsr      endela
031500010618     C                   endif
031600010330     C*
031700010330     C                   ENDSR
031800010330     C***
031900010330
032000010601
032100010601
032200010601
032300010601
032400010330     C*----------------------------------------------------*
032500010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
032600010330     C*----------------------------------------------------*
032700040607     C     INZVAR        BEGSR
032800010330     C*
032900020204     C                   Z-ADD     *zeros        Num5_0            5 0
033000020322     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
033100041011     C                   MOVEL     '0'           FlgCAS            1
033200041011     C* Imposto un flag che mi indica se ho già elaborato almeno 1 record del file del cliente
033300041011     C                   MOVEL     'N'           wGiro             1
033400041012     C* Imposto un flag che mi indica se trattasi d bolla da considerare (condiziona write del VAB)
033500041012     C                   MOVEL     'S'           wWrtVAB           1
033600010330     C*
033700010330     C                   ENDSR
033800010330     C*----------------------------------------------------*
033900040607     C*  IMPOSTAZIONE CAMPI COSTANTI
034000010330     C*----------------------------------------------------*
034100010330     C     DEFCAM        BEGSR
034200010330     C*
034300020204     C* Inizializzo il buffer del record da scrivere
034400040723     C                   CLEAR                   FIVAB000
034500020204     C* Imposto i valori di default...
034600100706     C                   EVAL      VABCCM = 2494479
034700040928     C                   EVAL      VABLNP = 249
034800100831     C                   EVAL      VABCTR = 001
034900060504     C                   EVAL      VABCTM = '7Q'
035000020204     C* ... e poi verifico se sono stati passati come parametri
035100020204     C                   IF        vlrppt > *blanks
035200040607     C                   IF        %subst(vlrppt:1:7) <> *blanks
035300020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
035400020204     C                   EXSR      CHKNUM
035500020204     C                   IF        PiInt=*on
035600020204     C                   Z-ADD     PiVal         VABCCM
035700020204     C                   ENDIF
035800040607     C                   ENDIF
035900040607     C                   IF        %subst(vlrppt:8:3) <> *blanks
036000020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
036100020204     C                   EXSR      CHKNUM
036200020204     C                   IF        PiInt=*on
036300020204     C                   Z-ADD     PiVal         VABLNP
036400020204     C                   ENDIF
036500040607     C                   ENDIF
036600040607     C                   IF        %subst(vlrppt:11:3) <> *blanks
036700020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
036800020204     C                   EXSR      CHKNUM
036900020204     C                   IF        PiInt=*on
037000020204     C                   Z-ADD     PiVal         VABCTR
037100020204     C                   ENDIF
037200040607     C                   ENDIF
037300020204     C                   ENDIF
037400020204     C*
037500010330     C                   ENDSR
037600010607     C*----------------------------------------------------*
037700040723     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
037800010607     C*----------------------------------------------------*
037900010607     C     IMPVAB        BEGSR
038000010607     C*
038100040928     C* Reperimento campi ALFA a seconda del tipo record
038200040928     C                   IF        %trim(%subst(vindta:1:3)) = 'SWS'
038300041011     C*
038400041011     C* Ad ogni nuova spedizione scarico il buffer d quella precedente
038500100824     C                   EXSR      WRIVAB
038600041011     C*
038700040928     C                   exsr      inzvar
038800040928     C                   exsr      defcam
038900040928     C                   MOVEL     DATCOR        VABAAS
039000040928     C                   MOVE      DATCOR        VABMGS
039100100706     C                   SETOFF                                       3132
039200100706     C                   MOVEL     'S'           wGiro
039300100706     C*
039400100706     C* ......VABNSP/VATNSP
039500100706     C* NSP => Stacco un numeratore da AZNUM
039600100706     C                   clear                   TRUL33DS
039700100706     C                   eval      I33OPE = *zeros
039800100706     C                   eval      I33CNU = 302
039900100706     C                   eval      I33NUM = 1
040000100706     C                   movel     TRUL33DS      KPJBU
040100100706     C                   call      'TRUL33R'
040200100706     C                   parm                    KPJBA
040300100706     C                   movel     KPJBU         TRUL33DS
040400100706     C                   if        O33ERR = *zeros
040500100706     C                   z-add     O33NRF        VABNSP
040600100706     C                   z-add     O33NRF        VATNSP
040700100706     C                   else
040800100706     C                   seton                                        31
040900100706     C                   EVAL      vinmsg = %trimr(vinmsg)
041000100706     C                             + ' ' + 'VABNSP VATNSP'
041100100706     C                   endif
041200100706     C*
041300040928     C                   ENDIF
041400040928     C*
041500100706     C* Tipo record => RT20FF
041600041011     C                   IF        %trim(%subst(vindta:1:7)) = 'RT20FF'
041700040928     C* RMA
041800100706     C                   EVAL      VABRMA=%trim(%subst(vindta:8:35))
041900100706     C                   ENDIF
042000040928     C*
042100100706     C* Tipo record => RT20AEG
042200101105     C                   IF        %trim(%subst(vindta:1:7)) = 'RT20AEG'
042300101105     C* RMN
042400110412     C                   IF        %subst(vindta:51:8) <> *blanks
042500101105     C                   EVAL      PiStr=%trim(%subst(vindta:51:8))
042600101105     C                   EXSR      CHKNUM
042700101105     C                   IF        PiInt=*on
042800101105     C                   Z-ADD     PiVal         VABRMN
042900101105     C                   ELSE
043000101105     C                   SETON                                        32
043100101105     C                   Z-ADD     1             VABRMN
043200101105     C                   EVAL      vinmsg = %trimr(vinmsg)
043300101105     C                             + ' ' + 'VABRMN'
043400101105     C                   ENDIF
043500040928     C                   ENDIF
043600110412     C                   ENDIF
043700040928     C*
043800100706     C* Tipo record => RT40CN
043900040928     C                   IF        %trim(%subst(vindta:1:7)) = 'RT40CN'
044000040928     C* RSD
044100040928     C                   EVAL      VABRSD=%trim(%subst(vindta:28:35))
044200040928     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
044300040928     C     '@':'A'       XLATE     VABRSD        VABRSD
044400040928     C* ==
044500040928     C* RD2
044600040928     C                   EVAL      VABRD2=%trim(%subst(vindta:63:35))
044700040928     C* IND
044800040928     C                   EVAL      VABIND=%trim(%subst(vindta:98:35))
044900040928     C* NT2
045000040928     C                   EVAL      VABNT2=%trim(%subst(vindta:133:35))
045100040928     C* LOD
045200040928     C                   EVAL      VABLOD=%trim(%subst(vindta:168:35))
045300040928     C* CAD
045400040928     C                   EVAL      PiStr=%trim(%subst(vindta:203:5))
045500040928     C                   EXSR      CHKNUM
045600040928     C                   IF        PiInt=*on
045700040928     C                   Z-ADD     PiVal         Num5_0
045800040928     C                   MOVEL(p)  Num5_0        VABCAD
045900040928     C                   ELSE
046000040928     C                   SETON                                        32
046100040928     C                   EVAL      VABCAD = *zeros
046200040928     C                   EVAL      vinmsg = %trimr(vinmsg)
046300040928     C                             + ' ' + 'VABCAD'
046400040928     C                   ENDIF
046500040928     C* Reperisco la provincia dal CAP e dalla località
046600040928     C                   IF        VABCAD <> *blanks
046700040928     C                   CLEAR                   TISI95DS
046800040928     C                   EVAL      I95TCN = '3'
046900040928     C                   Z-ADD     datcor        I95DAT
047000040928     C                   EVAL      I95CAP = VABCAD
047100040928     C                   EVAL      I95LOC = VABLOD
047200040928     C                   CALL      'TISI95R'
047300040928     C                   PARM                    TISI95DS
047400040928     C                   EVAL      VABPRD = O95PRV
047500040928     C                   ENDIF
047600040928     C                   ENDIF
047700041015     C*
047800100706     C* Tipo record => RT40DP
047900041015     C* Se presente il tipo record 'RT40DP' by-passa il tipo record 'RT40CN'...  x cui:
048000041015     C                   IF        %trim(%subst(vindta:1:7)) = 'RT40DP'
048100041015     C* RSD
048200041015     C                   EVAL      VABRSD=%trim(%subst(vindta:28:35))
048300041015     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
048400041015     C     '@':'A'       XLATE     VABRSD        VABRSD
048500041015     C* ==
048600041015     C* RD2
048700041015     C                   EVAL      VABRD2=%trim(%subst(vindta:63:35))
048800041015     C* IND
048900041015     C                   EVAL      VABIND=%trim(%subst(vindta:98:35))
049000041015     C* NT2
049100041015     C                   EVAL      VABNT2=%trim(%subst(vindta:133:35))
049200041015     C* LOD
049300041015     C                   EVAL      VABLOD=%trim(%subst(vindta:168:35))
049400041015     C* CAD
049500041015     C                   EVAL      PiStr=%trim(%subst(vindta:203:5))
049600041015     C                   EXSR      CHKNUM
049700041015     C                   IF        PiInt=*on
049800041015     C                   Z-ADD     PiVal         Num5_0
049900041015     C                   MOVEL(p)  Num5_0        VABCAD
050000041015     C                   ELSE
050100041015     C                   SETON                                        32
050200041015     C                   EVAL      VABCAD = *zeros
050300041015     C                   EVAL      vinmsg = %trimr(vinmsg)
050400041015     C                             + ' ' + 'VABCAD'
050500041015     C                   ENDIF
050600041015     C* Reperisco la provincia dal CAP e dalla località
050700041015     C                   IF        VABCAD <> *blanks
050800041015     C                   CLEAR                   TISI95DS
050900041015     C                   EVAL      I95TCN = '3'
051000041015     C                   Z-ADD     datcor        I95DAT
051100041015     C                   EVAL      I95CAP = VABCAD
051200041015     C                   EVAL      I95LOC = VABLOD
051300041015     C                   CALL      'TISI95R'
051400041015     C                   PARM                    TISI95DS
051500041015     C                   EVAL      VABPRD = O95PRV
051600041015     C                   ENDIF
051700041015     C                   ENDIF
051800040928     C*
051900100706     C* Tipo record => RT50001
052000040928     C                   IF        %trim(%subst(vindta:1:7)) = 'RT50001'
052100040928     C* NCL
052200040928     C                   EVAL      PiStr=%trim(%subst(vindta:8:8))
052300040928     C                   EXSR      CHKNUM
052400040928     C                   IF        PiInt=*on
052500040928     C                   Z-ADD     PiVal         VABNCL
052600040928     C                   ELSE
052700040928     C                   SETON                                        32
052800040928     C                   Z-ADD     *zeros        VABNCL
052900040928     C                   EVAL      vinmsg = %trimr(vinmsg)
053000040928     C                             + ' ' + 'VABNCL'
053100040928     C                   ENDIF
053200040928     C* PKB
053300040928     C                   EVAL      PiStr=%trim(%subst(vindta:64:18))
053400040928     C                   EXSR      CHKNUM
053500040928     C                   IF        PiNum=*on
053600040928     C                   Z-ADD     PiVal         VABPKB
053700040928     C                   ELSE
053800040928     C                   SETON                                        32
053900040928     C                   Z-ADD     *zeros        VABPKB
054000040928     C                   EVAL      vinmsg = %trimr(vinmsg)
054100040928     C                             + ' ' + 'VABPKB'
054200040928     C                   ENDIF
054300040928     C                   ENDIF
054400041012     C*
054500100706     C* Tipo record => RT70
054600100824     C***                IF        %trim(%subst(vindta:1:4)) = 'RT70'
054700100824     C***                IF        %trim(%subst(vindta:50:2)) = '11' OR
054800100824     C***                          %trim(%subst(vindta:50:2)) = '43' OR
054900100824     C***                          %trim(%subst(vindta:50:2)) = '44'
055000100824     C***                EVAL      wWrtVAB = 'N'
055100100824     C***                ENDIF
055200100824     C***                ENDIF
055300100824     C*
055400100824     C* Tipo record => RT10785
055500100824     C                   IF        %trim(%subst(vindta:1:7)) = 'RT10785'
055600100824     C                   EVAL      wWrtVAB = 'N'
055700100824     C                   ENDIF
055800040928     C*
055900100706     C* Tipo record => RT80
056000040928     C                   IF        %trim(%subst(vindta:1:4)) = 'RT80'
056100040928     C                   IF        %trim(%subst(vindta:51:3)) = '50'
056200040928     C                   EVAL      FlgCAS = '1'
056300110406     C* - gestione particolarità x traffico 2494613
056400110406     C                   IF        VABCCM = 2494613
056500110406     C                   EVAL      VABTIC = 'OM'
056600110406     C                   EVAL      VABRMO = 'BOUGIES LA FRANCAISE'
056700110406     C                   ENDIF
056800111018     C* - gestione particolarità x traffico 2494678
056900111018     C                   IF        VABCCM = 2494678
057000111018     C                   EVAL      VABTIC = 'OM'
057100111018     C                   EVAL      VABRMO = 'ABT'
057200111018     C                   ENDIF
057300040928     C* CAS
057400040928     C                   EVAL      PiStr=%trim(%subst(vindta:54:18))
057500040928     C                   EXSR      CHKNUM
057600040928     C                   IF        PiNum=*on
057700040928     C                   Z-ADD     PiVal         VABCAS
057800040928     C                   ELSE
057900040928     C                   SETON                                        32
058000040928     C                   Z-ADD     *zeros        VABCAS
058100040928     C                   EVAL      vinmsg = %trimr(vinmsg)
058200040928     C                             + ' ' + 'VABCAS'
058300040928     C                   ENDIF
058400040928     C* VCA
058500040928     C                   EVAL      VABVCA=%trim(%subst(vindta:72:3))
058600040928     C                   ENDIF
058700040928     C                   ENDIF
058800060504     C*
058900100706     C* Tipo record => RT20LS
059000060504     C* Reperisco il barcode sul tipo record 'RT20LS'
059100100824     C                   IF        %trim(%subst(vindta:1:7)) = 'RT20LS' AND
059200100824     C                             wWrtVAB = 'S'
059300100706     C                   EVAL      VATFGS = VABFGS
059400060504     C                   EVAL      VATCCM = VABCCM
059500060504     C                   EVAL      VATAAS = VABAAS
059600060504     C                   EVAL      VATLNP = VABLNP
059700060504     C                   EVAL      VATNSP = VABNSP
059800060504     C                   EVAL      VATTRC = 'E'
059900100707     C                   EVAL      VATNOT='00'+%trim(%subst(vindta:8:35))
060000110421     C* - gestione particolarità x traffico 2494613
060100110421     C                   IF        VATCCM = 2494613
060200110421     C                   EVAL      VATNOT=%trim(%subst(vindta:8:35))
060300110421     C                   ENDIF
060400060504     C                   WRITE     FIVAT000
060500060504     C                   ENDIF
060600020204     C*
060700010607     C                   ENDSR
060800010607     C*----------------------------------------------------*
060900100824
061000100824
061100100824     C*----------------------------------------------------*
061200100824     C*  SCARICO BUFFER FILE TESTATA - FNVAB
061300100824     C*----------------------------------------------------*
061400100824     C     WRIVAB        BEGSR
061500100824     C*
061600100824     C                   IF        wGiro = 'S' AND wWrtVAB = 'S'
061700100824     C* Considerazioni finali su CBO/CAS
061800100824     C                   IF        FlgCAS = '1'
061900100824     C                   EVAL      VABCBO = '4'
062000100824     C                   ELSE
062100100824     C                   EVAL      VABCBO = '1'
062200100824     C                   ENDIF
062300100824     C* Eseguo routine finale x considerazioni specifiche su importi/divise
062400100824     C                   EXSR      CHKIMPDIV
062500100824     C* Scarico il buffer del FIVAB
062600100824     C  N31              WRITE     FIVAB000
062700100824     C                   ENDIF
062800100824     C*
062900100824     C                   ENDSR
063000100824     C***
063100010601
063200020204
063300020204     C*----------------------------------------------------*
063400020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
063500020204     C*----------------------------------------------------*
063600020204     C     CHKIMPDIV     BEGSR
063700020204     C*
063800020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
063900020204     C                   Z-ADD     *zeros        wrkDec            9 9
064000020204     C*
064100020204     C* Come prima cosa effettuo considerazioni sulla divisa
064200020204     C                   IF        vabIAS > *zeros
064300020204     C                   IF        vabVAS <> 'EUR'
064400020204     C                   EVAL      vabVAS =  'ITL'
064500020204     C                   ENDIF
064600020204     C                   ENDIF
064700020204     C*
064800020204     C                   IF        vabCAS > *zeros
064900020204     C                   IF        vabVCA <> 'EUR'
065000020204     C                   EVAL      vabVCA =  'ITL'
065100020204     C                   ENDIF
065200020204     C                   ENDIF
065300020204     C*
065400020204     C                   IF        vabVMD > *zeros
065500020321     C                   IF        vabVAD <> 'EUR'
065600020204     C                   EVAL      vabVAD =  'ITL'
065700020204     C                   ENDIF
065800020204     C                   ENDIF
065900020204     C*
066000020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
066100020204     C                   Z-ADD     vabIAS        wrkDec
066200020204     C                   IF        wrkDec > *zeros
066300020204     C                   IF        vabVAS = 'ITL'
066400020204     C                   EVAL      vabIAS = *zeros
066500020204     C                   ENDIF
066600020204     C                   ENDIF
066700020204     C*
066800020204     C* Stabilisco se il contrasegno ha decimali valorizzati
066900020204     C                   Z-ADD     vabCAS        wrkDec
067000020204     C                   IF        wrkDec > *zeros
067100020204     C                   IF        vabVCA = 'ITL'
067200020204     C                   EVAL      vabCAS = *zeros
067300020204     C                   ENDIF
067400020204     C                   ENDIF
067500020204     C*
067600020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
067700020204     C                   Z-ADD     vabVMD        wrkDec
067800020204     C                   IF        wrkDec > *zeros
067900020204     C                   IF        vabVAD = 'ITL'
068000020204     C                   EVAL      vabVMD = *zeros
068100020204     C                   ENDIF
068200020204     C                   ENDIF
068300020204     C*
068400020204     C                   ENDSR
068500020204     C***
068600020204
068700060504
068800060504     C*----------------------------------------------------*
068900060504     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
069000060504     C*----------------------------------------------------*
069100060504     C     PREVAT        BEGSR
069200060504     C*
069300060504     C* Compongo il nome del membro da dare al FIVATWWR
069400060504     C                   eval      parmbr = vlrhdl
069500060504     C                   movel     'M'           parmbr
069600060504     C                   eval      parccm = vlrksc
069700060504     C                   eval      paropz = '1'
069800060504     C* Effettuo la chiamata al CLLE preposto
069900060504     C                   call(e)   'TITVVTC'
070000060504     C                   parm                    parccm
070100060504     C                   parm                    parmbr
070200060504     C                   parm                    paropz
070300060504     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
070400060504     C                   if        %error
070500060504     C                   movel     '1'           chkcall
070600060504     C                   else
070700060504     C                   movel     '0'           chkcall
070800060504     C                   endif
070900060504     C*
071000060504     C                   ENDSR
071100060504     C*----------------------------------------------------*
071200060504
071300010330
071400010330     C*----------------------------------------------------*
071500010330     C*  CONTROLLO NUMERICITA' CAMPI
071600010330     C*----------------------------------------------------*
071700010330     C     CHKNUM        BEGSR
071800010330     C*
071900010606     C                   IF        PiDecChr = *blanks
072000040928     C                   EVAL      PiDecChr = '.'
072100010606     C                   ENDIF
072200010606     C*
072300010606     C                   CALL(e)   'ISNUMERIC'
072400010330     C                   PARM                    PiStr            30
072500010606     C                   PARM                    PiDecChr          1
072600010330     C                   PARM      *ZEROS        PiVal            30 9
072700010330     C                   PARM      '0'           PiInt             1
072800010330     C                   PARM      '0'           PiNum             1
072900010330     C                   IF        %error
073000010606     C                   EVAL      PiNum=*off
073100010330     C                   ENDIF
073200010330     C*
073300010330     C                   ENDSR
073400010330     C***
073500010330
073600010601
073700010601
073800010601
073900010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
074000010601     C     repfil        BEGSR
074100010601     C*
074200010601     C                   if        invfil = *zeros and
074300010601     C                             depfil > *zeros and
074400010629     C                             (vinflg = *blanks or
074500010629     C                              vinflg = *zeros)
074600010601     C
074700010601     C                   eval      invfil = depfil
074800010601     C                   endif
074900010601     C*
075000010601     C                   if        depfil <> invfil and
075100010601     C                             invfil > *zeros
075200010601     C                   eval      flgMulti = '1'
075300010601     C                   if        vinflg = *blanks
075400010601     C                   add       1             cntNonEl
075500010601     C                   endif
075600010601     C                   endif
075700010601     C*
075800010601     C                   if        vinflg = '2'
075900010601     C                   eval      flgStato = '2'
076000010601     C                   endif
076100010601     C*
076200010601     C                   ENDSR
076300010601     C***
076400010601
076500010330
076600010330
076700010330
076800060504      /TITLE Invio dei dati al punto operativo.
076900060504     C     invio         BEGSR
077000060504     C*
077100060504     C* 1° invio FIVAT
077200060504     C                   reset                   dscmz
077300060504     C                   move      vlrpoi        cmzdst
077400060504     C                   eval      cmzfld = 'FIVATWWR'
077500060504     C                   eval      cmzmbd = vlrhdl
077600060504     C                   eval      %subst(cmzmbd:1:1) = 'M'
077700060504     C***                if        prmfir = *blanks
077800060504     C                   eval      cmzfla = 'FIVAT00F'
077900060504     C                   eval      cmzmba = 'FIVAT00F'
078000060504     C***                else
078100060504     C***                eval      cmzfla = prmfir
078200060504     C***                eval      cmzmba = prmfir
078300060504     C***                endif
078400060504     C                   eval      cmznrr = *zeros
078500060504     C                   move      §ctrokvt      cmznrr
078600060504     C                   eval      cmzlba = vlrfl1
078700060504     C                   call(e)   'TIS711C'
078800060504     C                   parm                    dscmz
078900060504     C                   parm      *blanks       esito
079000060504     C                   if        %error
079100060504     C                             or cmzerr = '1'
079200060504     C                             or esito  = '1'
079300060504     C                   eval      wrkesito = '3'
079400060504     C                   else
079500060504     C*
079600060504     C* 2° invio FIVAB
079700060504     C                   reset                   dscmz
079800060504     C                   move      vlrpoi        cmzdst
079900060504     C                   eval      cmzfld = vlrfou
080000060504     C                   eval      cmzmbd = vlrhdl
080100060504     C                   eval      %subst(cmzmbd:1:1) = 'M'
080200060504     C***                if        prmfir = *blanks
080300060504     C                   eval      cmzfla = 'FIVAB00F'
080400060504     C                   eval      cmzmba = 'FIVAB00F'
080500060504     C***                else
080600060504     C***                eval      cmzfla = prmfir
080700060504     C***                eval      cmzmba = prmfir
080800060504     C***                endif
080900060504     C                   eval      cmznrr = *zeros
081000060504     C                   move      §ctrok        cmznrr
081100060504     C                   eval      cmzlba = vlrfl1
081200060504     C                   call(e)   'TIS711C'
081300060504     C                   parm                    dscmz
081400060504     C                   parm      *blanks       esito
081500060504     C                   if        %error
081600060504     C                             or cmzerr = '1'
081700060504     C                             or esito  = '1'
081800060504     C                   eval      wrkesito = '3'
081900060504     C                   endif
082000060504     C                   endif
082100060504     C*
082200060504     C                   ENDSR
082300060504     C***
082400990910
082500010601
082600010601
082700010601
082800010601      /TITLE Invio dei dati al punto operativo.
082900010601     C     opeini        BEGSR
083000010601     C*
083100010601     C* Inizializzo flag e contatori operativi
083200010601     C                   movel     '0'           flgGiro           1
083300010601     C                   movel     '0'           flgMulti          1
083400010601     C                   movel     '1'           flgStato          1
083500010615     C                   movel     '0'           flgOk             1
083600010601     C                   z-add     *zeros        cntNonEl         10 0
083700010601     C                   z-add     *zeros        depfil            3 0
083800010601     C                   z-add     *zeros        invfil            3 0
083900010601     C*
084000010601     C                   ENDSR
084100010601     C***
084200010601
084300010601
084400010330
084500010330
084600000613     C     *inzsr        BEGSR
084700990910     C*
084800990910     C     *entry        plist
084900990920     C                   parm                    tivlrds
085000990921     C                   parm      wrkesito      esito
085100000724     C                   parm                    prmlit
085200000710     C                   parm                    prmfir
085300010330     C*
085400010330     C* CALCOLA LA DATA CORRENTE
085500100706     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
085600100706     C                   eval      datcor = %dec(%date() : *iso)
085700000613     C*
085800000613     C                   ENDSR
085900000613     C***
086000990908
086100040928     Otitve8p   E            riepilogo         2
086200990915     O                                              'Upload via Internet'
086300990915     O                                           +1 'Traduzione TIVIN00R -'
086400060504     O                                           +1 'FIVABWWR/FIVATWWR'
086500990915     O                                           +1 'Testate bolle'
086600990915     O          E            riepilogo   2
086700990915     O                       wrkdata
086800990915     O                       wrkora              +1
086900990915     O                       procname            +1
087000990915     O          E            riepilogo   2
087100990915     O                                              'Cliente..................:'
087200010605     O                       VABCCM        z     +1
087300990915     O          E            riepilogo   2
087400990920     O                                              'Riferimento Strategi.....:'
087500990920     O                       vlrhdl              +1
087600990915     O          E            riepilogo   2
087700990915     O                                              'Giusti...................:'
087800971022     O                       §CTROK        2   +  1
087900990915     O          E            riepilogo   2
088000990915     O                                              'Sbagliati e corretti.....:'
088100971022     O                       §CTRMO        2   +  1
088200990915     O          E            riepilogo   2
088300990915     O                                              'Sbagliati e scartati.....:'
088400971022     O                       §CTRNO        2   +  1
088500000613
088600040928     Otitve8ps  E            testdett          2
088700000613     O                                              'Upload via Internet'
088800000613     O                                           +1 'Traduzione TIVIN00R -'
088900060504     O                                           +1 'FIVABWWR/FIVATWWR'
089000000613     O                                           +1 'Testate bolle'
089100000616     O          E            testdett    3
089200000613     O                                           +2 'N° rec'
089300000613     O                                           +3 'Anteprima contenuto'
089400000616     O          E            rigadett    2
089500000613     O                       rrnum               +2
089600000621     O                       recko               +3
089700000616     O          E            findett     2
089800000613     O                       wrkdata
089900000613     O                       wrkora              +1
090000000613     O                       procname            +1
090100000616     O          E            findett     2
090200000613     O                                              'Cliente..................:'
090300010605     O                       VABCCM        z     +1
090400000616     O          E            findett     2
090500000613     O                                              'Riferimento Strategi.....:'
090600000613     O                       vlrhdl              +1
090700000616     O          E            findett     2
090800000613     O                                              'Giusti...................:'
090900000613     O                       §CTROK        2   +  1
091000000616     O          E            findett     2
091100000613     O                                              'Sbagliati e corretti.....:'
091200000613     O                       §CTRMO        2   +  1
091300000616     O          E            findett     2
091400000613     O                                              'Sbagliati e scartati.....:'
091500000613     O                       §CTRNO        2   +  1
091600000613** CMD - COMANDI CL
091700040928OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVE8P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
091800040928OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVE8PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
091900040928DLTOVR FILE(TITVE8P TITVE8PS) LVL(*)
092000010606
092100000613
