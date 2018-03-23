000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021113     FFIVABwwr  O    E             DISK    usropn
000700021113     FFIVATwwr  O    E             DISK    usropn
000800070719     Ftitvz7p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100070719     Ftitvz7ps  O    f  198        PRINTER usropn
001200000621     F                                     oflind(*inob)
001300070411     F                                     infsr(*pssr)
001400990908
001500000512     D*------------
001600000512     D* COMANDI
001700000512     D*------------
001800011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001900000801     D*----------------------------------------------------
002000000801     D* DICHIARAZIOINE VARIABILI DI WRK
002100000801     D*----------------------------------------------------
002200990920     D dscmz         e ds                  inz
002300990910     D psds           sds
002400990910     D  procname         *PROC
002500990920     D tivlrds       e ds                  extname(tivlr00f)
002501070719     D tisi95ds      e ds
002600990910     D esito           s              1
002700000724     D prmlit          s             10
002800000710     D prmfir          s             10
002900990921     D wrkesito        s                   like(esito)
003000990915     D wrkdata         s               d
003100990915     D wrkora          s               t
003200000613     D rrnum           s              6  0 INZ(*zeros)
003300000621     D recko           s            150    INZ(*blanks)
003400011113     D depcmd          s            150    INZ(*blanks)
003500010202     D parccm          s              8    INZ(*blanks)
003600010202     D parmbr          s             10    INZ(*blanks)
003700010202     D paropz          s              1    INZ(*blanks)
003800010202     D chkcall         s              1    INZ(*blanks)
003900000830
004000000830     D*------------------
004100000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
004200000830     D*------------------
004300000830     D WLBDA8          DS                  INZ
004400000830     D  G08DAT                 1      8  0
004500000830     D  G08INV                 9     16  0
004600000830     D  G08ERR                17     17
004700000830     D  G08TGI                18     22  0
004800041025     D*------------------
004900041025     D* DS REPERIMENTO NUMERATORE
005000041025     D*------------------
005100041025     D trul33ds      e ds                  inz
005200041025     D*------------------
005300041025     D* DS ARCHITETTURA
005400041025     D*------------------
005500041025     D kpjba         e ds                  inz
005600041025     D*------------------
005700990908
005800010201
005900010201
006000990915     C                   time                    wrkdata
006100990915     C                   time                    wrkora
006200000913     C                   reset                   rrnum
006300990921     C                   reset                   esito
006400990921     C                   reset                   wrkesito
006500000724     C*
006600000724     C* SE OCCORRE SPEDIRE IN FILIALE
006700000724     C                   if        vlrpoi <> *zeros
006800000724     C*
006900000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
007000000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007100000724     C     vlrpoi        chain     azorg01l
007200000724     C                   if        %found
007300000616     C                   movel(p)  CMD(1)        depcmd
007400020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
007500000616     C*
007600000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
007700011113     C                   Z-ADD     150           LENGH            15 5
007800000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
007900000616     C                   PARM                    depcmd
008000000616     C                   PARM                    LENGH
008100000724     C*
008200000724     C                   endif
008300000724     C                   endif
008400000616     C*
008500000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008600000616     C                   movel(p)  CMD(2)        depcmd
008700000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
008800000616     C*
008900000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
009000011113     C                   Z-ADD     150           LENGH            15 5
009100000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009200000616     C                   PARM                    depcmd
009300000616     C                   PARM                    LENGH
009400000616     C*
009500070719     C                   if        not %open(titvz7ps)
009600070719     C                   open      titvz7ps
009700000616     C                   except    testdett
009800000613     C                   endif
009900000613     C*
010000040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
010100000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
010101070719     C*
010102070719     C* Esegue lancio TISI95R solo x chiusura
010103070719     C                   CLEAR                   TISI95DS
010104070719     C                   EVAL      I95TLA = 'C'
010105070719     C                   CALL      'TISI95R'
010106070719     C                   PARM                    TISI95DS
010200000613     C*
010300010202     C* Effettuo la chiamata al CLLE preposto
010400040506     C                   call(e)   'TITVVTC'
010500010202     C                   parm                    parccm
010600010202     C                   parm                    parmbr
010700010202     C                   parm      '2'           paropz
010800010202     C*
010900070719     C                   if        %open(titvz7ps)
011000000616     C                   except    findett
011100070719     C                   close     titvz7ps
011200000613     C                   endif
011300000616     C*
011400000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
011500011113     C                   Z-ADD     150           LENGH            15 5
011600000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
011700000616     C                   PARM                    CMD(3)
011800000616     C                   PARM                    LENGH
011900000616     C*
012000010201     C                   seton                                        LR
012100000613
012200000613
012300000613     C*--------------------------------------------------------
012400000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
012500000613     C*--------------------------------------------------------
012600000621     C     STPR          BEGSR
012700000613     C*
012800070719     C                   if        not %open(titvz7p)
012900070719     C                   open      titvz7p
013000990915     C                   endif
013100990915     C*
013200990915     C                   except    riepilogo
013300990915     C*
013400070719     C                   if        %open(titvz7p)
013500070719     C                   close     titvz7p
013600990914     C                   endif
013700990910     C*
013800000613     C                   ENDSR
013900000613     C***
014000990908
014100000801
014200910830     C*--------------------------------------------------------
014300040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
014400910830     C*--------------------------------------------------------
014500040526     C     RWFILE        BEGSR
014600990910     C*
014700990914     C                   if        not %open(tivin00r)
014800990908     C                   open      tivin00r
014900990914     C                   endif
015000021113     C                   if        not %open(fivabwwr)
015100021113     C                   open      fivabwwr
015200990914     C                   endif
015300021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
015400020305     C                   exsr      prevat
015500010201     C*
015600010202     C                   if        chkcall = '0'
015700010202     C*
015800021113     C                   if        not %open(fivatwwr)
015900021113     C                   open      fivatwwr
016000010201     C                   endif
016100990910     C*
016200010201     C                   clear                   §CTROKVB          5 0
016300020305     C                   clear                   §CTROKVT          5 0
016400000801     C                   clear                   §CTRMO            5 0
016500000801     C                   clear                   §CTRNO            5 0
016600040910     C*
016700921023     C                   DO        *HIVAL
016800990913     C*
016900990915     C                   READ      tivin00r                               70
017000040910     C                   if        vindta > *blanks
017100000613     C                   add       1             rrnum
017200000801     C*
017300000801     C                   if        *in70 = *off
017400000801     C                             and
017500000801     C                             (vinflg = *blanks
017600000801     C                              or vinflg = '0'
017700000801     C                              or vinflg = '2')
017800000801     C*
017900000801     C                   clear                   vinmsg
018000000801     C                   eval      vinflg = '1'
018100040910     C*
018200040910     C* Eseguo routine d traduzione
018300040910     C                   exsr      impvabvat
018400040802     C*
018500010305     C                   endif
018600000905     C*
018700000905     C                   else
018800000905     C                   eval      vinflg = '1'
018900000905     C                   endif
019000000905     C*
019100000905     C  N70              update    tivin000
019200000905     C*
019300991022     C  N70              ENDdo
019400010202     C*
019500010202     C                   endif
019600990910
019700990910     C* Se non ci sono record con errori ...
019800000710     C                   if        §ctrno = 0
019900990910     C* ... restituisco esito OK.
020000990921     C                   eval      wrkesito = '0'
020100990910     C                   else
020200010201     C                   if        §ctrokvb > 0
020300990921     C                   eval      wrkesito = '1'
020400000710     C                   else
020500000710     C                   eval      wrkesito = '2'
020600990910     C                   endif
020700000710     C                   endif
020800990910     C*
020900990914     C                   if        %open(tivin00r)
021000990908     C                   close     tivin00r
021100990914     C                   endif
021200021113     C                   if        %open(fivabwwr)
021300021113     C                   close     fivabwwr
021400990914     C                   endif
021500021113     C                   if        %open(fivatwwr)
021600021113     C                   close     fivatwwr
021700010201     C                   endif
021800990910     C*
021900010201     C                   if        §ctrokvb > 0
022000000724     C                             and vlrpoi <> *zeros
022100010202     C                   exsr      invio
022200990920     C                   endif
022300990920     C*
022400910830     C                   ENDSR
022500000613     C***
022600990920
022700000801     C*----------------------------------------------------*
022800000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
022900000801     C*----------------------------------------------------*
023000010201     C     INZVAR        BEGSR
023100000801     C*
023200040802     C                   Z-ADD     *zeros        Num5_0            5 0
023300040802     C                   MOVEL     '0'           FlgCAS            1
023400000801     C*
023500000801     C                   ENDSR
023600000801     C*----------------------------------------------------*
023700040910     C*  IMPOSTAZIONE CAMPI COSTANTI
023800000801     C*----------------------------------------------------*
023900000801     C     DEFCAM        BEGSR
024000000801     C*
024100021113     C                   CLEAR                   FIVAB000
024200040802     C                   CLEAR                   FIVAT000
024300020619     C* Imposto i valori di default...
024400070719     C                   Z-ADD     0054306       VABCCM
024500070719     C                   Z-ADD     0054306       VATCCM
024600070719     C                   Z-ADD     005           VABLNP
024700070719     C                   Z-ADD     005           VATLNP
024800070531     C                   Z-ADD     000           VABCTR
024801070531     C                   MOVEL     '7Q'          VABCTM
024900040823     C                   MOVEL     '1'           VABCBO
025000020619     C* ... e poi verifico se sono stati passati come parametri
025100020619     C                   IF        vlrppt > *blanks
025200040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
025300020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
025400020619     C                   EXSR      CHKNUM
025500020619     C                   IF        PiInt=*on
025600020619     C                   Z-ADD     PiVal         VABCCM
025700020619     C                   Z-ADD     PiVal         VATCCM
025800020619     C                   ENDIF
025900040506     C                   ENDIF
026000040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
026100020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
026200020619     C                   EXSR      CHKNUM
026300020619     C                   IF        PiInt=*on
026400020619     C                   Z-ADD     PiVal         VABLNP
026500020619     C                   Z-ADD     PiVal         VATLNP
026600040506     C                   ENDIF
026700020619     C                   ENDIF
026800040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
026900020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
027000020619     C                   EXSR      CHKNUM
027100020619     C                   IF        PiInt=*on
027200020619     C                   Z-ADD     PiVal         VABCTR
027300040506     C                   ENDIF
027400020619     C                   ENDIF
027500060202     C                   IF        %subst(vlrppt:14:2) <> *blanks
027600060202     C                   EVAL      VABCTM=%trim(%subst(vlrppt:14:2))
027700060202     C                   ENDIF
027800020619     C                   ENDIF
027900000801     C*
028000000801     C                   ENDSR
028100000801     C*----------------------------------------------------*
028200040910     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB e FIVAT)
028300000801     C*----------------------------------------------------*
028400040910     C     IMPVABVAT     BEGSR
028500040910     C*
028600040910     C* Traduzione relativa ai tipi record del file del cliente
028700040910     C*
028800040910     C***
028900070719     C* ...tipo record 'EBE' (testata)
029000070719     C                   IF        %subst(vindta:1:3) = 'EBE'
029001070719     C* Resetto indicatore d anomalia sul singolo record
029002070719     C                   eval      vinflg = '1'
029100040910     C* ......inizializzazioni iniziali e formati record file Bartolini
029200040910     C                   EXSR      INZVAR
029300040910     C                   EXSR      DEFCAM
029400040928     C*
029500040928     C                   Z-ADD     *zeros        errore            1 0
029600040928     C* ......valorizzazione campi da data traduzione e da P.O. d gestione
029700040928     C                   MOVEL     datcor        VABAAS
029800040928     C                   MOVEL     datcor        VATAAS
029900040928     C                   MOVE      datcor        VABMGS
030000040928     C                   MOVE(P)   vlrpoi        VABFGS
030100040928     C                   MOVE(P)   vlrpoi        VATFGS
030200040910     C* ......VABNSP/VATNSP
030300041025     C* NSP => Stacco un numeratore da AZNUM
030400041025     C                   clear                   TRUL33DS
030500041025     C                   eval      I33OPE = *zeros
030600041025     C                   eval      I33CNU = 302
030700041025     C                   eval      I33NUM = 1
030800041025     C                   movel     TRUL33DS      KPJBU
030900041025     C                   call      'TRUL33R'
031000041025     C                   parm                    KPJBA
031100041025     C                   movel     KPJBU         TRUL33DS
031200041025     C                   if        O33ERR = *zeros
031300041025     C                   z-add     O33NRF        VABNSP
031400041025     C                   z-add     O33NRF        VATNSP
031500041025     C                   else
031600041025     C                   Z-ADD     1             errore
031700041025     C                   EVAL      vinmsg = %trimr(vinmsg)
031800041025     C                             + ' ' + 'VABNSP VATNSP'
031900041025     C                   endif
032000060202     C* ......VABRMA
032100070719     C                   EVAL      VABRMA=%trim(%subst(vindta:22:15))
032200060202     C* ......VABRMN
032300070719     C                   EVAL      PiStr=%trim(%subst(vindta:22+1:15-1))
032400060202     C                   EXSR      CHKNUM
032500060202     C                   IF        PiInt=*on
032600060202     C                   Z-ADD     PiVal         VABRMN
032700060202     C                   ELSE
032800060202     C                   ADD       1             errore
032900060202     C                   EVAL      vinmsg = %trimr(vinmsg)
033000060202     C                             + ' ' + 'VABRMN'
033100060202     C                   ENDIF
033200060202     C* ......VABNCL
033300070719     C                   EVAL      PiStr=%trim(%subst(vindta:275:5))
033400060202     C                   EXSR      CHKNUM
033500060202     C                   IF        PiInt=*on
033600060202     C                   Z-ADD     PiVal         VABNCL
033700060202     C                   ELSE
033800060202     C                   ADD       1             errore
033900060202     C                   EVAL      vinmsg = %trimr(vinmsg)
034000060202     C                             + ' ' + 'VABNCL'
034100060202     C                   ENDIF
034200060202     C* ......VABPKB
034300070719     C                   EVAL      PiStr=%trim(%subst(vindta:280:9))
034400060202     C                   EXSR      CHKNUM
034500060202     C                   IF        PiNum=*on
034501070726     C                   EVAL      PiVal = PiVal / 1000                         da grammi a kg
034700070726     C                   Z-ADD(H)  PiVal         VABPKB
034800060202     C                   ELSE
034900060202     C                   ADD       1             errore
035000060202     C                   EVAL      vinmsg = %trimr(vinmsg)
035100060202     C                             + ' ' + 'VABPKB'
035200060202     C                   ENDIF
036400060202     C* ......VABRSD
036500070719     C                   EVAL      VABRSD=%trim(%subst(vindta:53:35))
036600060202     C* ......VABIND
036700070719     C                   EVAL      VABIND=%trim(%subst(vindta:88:35))
036701070719     C* ......VABRD2
036702070719     C                   EVAL      VABRD2=%trim(%subst(vindta:123:35))
036800060202     C* ......VABLOD
036900070719     C                   EVAL      VABLOD=%trim(%subst(vindta:237:35))
037000060202     C* ......VABCAD
037100070719     C                   EVAL      PiStr=%trim(%subst(vindta:228:9))
037200060202     C                   EXSR      CHKNUM
037300060202     C                   IF        PiInt=*on
037400060202     C                   Z-ADD     PiVal         Num5_0
037500060202     C                   MOVEL(P)  Num5_0        VABCAD
037600060202     C                   ELSE
037700060202     C                   ADD       1             errore
037800060202     C                   EVAL      vinmsg = %trimr(vinmsg)
037900060202     C                             + ' ' + 'VABCAD'
038000060202     C                   ENDIF
038100060202     C* ......VABPRD
038201070719     C* Reperisco la provincia dal CAP e dalla località
038202070719     C                   IF        VABPRD  = *blanks AND
038203070719     C                             VABCAD <> *blanks
038204070719     C                   CLEAR                   TISI95DS
038205070719     C                   EVAL      I95TCN = '3'
038206070719     C                   Z-ADD     datcor        I95DAT
038207070719     C                   EVAL      I95CAP = VABCAD
038208070719     C                   EVAL      I95LOC = VABLOD
038209070719     C                   CALL      'TISI95R'
038210070719     C                   PARM                    TISI95DS
038211070719     C                   EVAL      VABPRD = O95PRV
038212070719     C                   ENDIF
039800060202     C* ......VABNOT
039900070719     C                   EVAL      VABNOT=%trim(%subst(vindta:318:35))
040000060202     C* ......VABNT2
040100070719     C                   EVAL      VABNT2=%trim(%subst(vindta:353:35))
045500060202     C*
045600060202     C* Considerazioni sul contenuto di campi precedentemente valorizzati
045700060202     C                   IF        FlgCAS <> '0'
045800060202     C                   IF        VABCBO = '1'
045900060202     C                   EVAL      VABCBO = '4'
046000060202     C                   ELSE
046100060202     C                   EVAL      VABCBO = '6'
046200060202     C                   ENDIF
046300060202     C                   ENDIF
046400060202     C*
046500060202     C* Eseguo routine finale x considerazioni specifiche su importi/divise
046600060202     C                   EXSR      CHKIMPDIV
046700060202     C* ...e in fine...
046800060202     C                   WRITE     FIVAB000
046900040910     C*
047000040910     C                   ENDIF
047100040910     C***
047200070719     C* ...tipo record 'EBC' (dettaglio barcode segnacooli)
047300070719     C                   IF        %subst(vindta:1:3) = 'EBC'
047301070719     C* Resetto indicatore d anomalia sul singolo record
047302070719     C                   eval      vinflg = '1'
047400040910     C* ......VATNOT
047500070731     C                   EVAL      VATNOT=%trim(%subst(vindta:45:17))
047600040910     C                   EVAL      VATTRC = 'E'
047700070719     C* x ogni tipo record 'EBC' devo scaricare il buffer del file FIVAT
047800060202     C                   IF        VATNOT <> *blanks
047900040910     C                   WRITE     FIVAT000
048000060202     C                   ENDIF
048100040910     C*
048200040910     C                   ENDIF
048300010202     C*
048400000801     C* Ebbene...
048500000801     C                   ADD       1             §CTRMO
048600010201     C                   IF        errore <> *zeros
048700000801     C                   ADD       1             §CTRNO
048800000801     C                   EVAL      recko = vindta
048900000801     C                   EXCEPT    rigadett
049000000801     C                   EVAL      vinflg = '2'
049100000801     C                   ELSE
049200010201     C                   ADD       1             §CTROKVB
049300000801     C                   ENDIF
049400000801     C*
049500000801     C                   ENDSR
049600010202     C*----------------------------------------------------*
049700021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
049800010202     C*----------------------------------------------------*
049900020305     C     PREVAT        BEGSR
050000010202     C*
050100021113     C* Compongo il nome del membro da dare al FIVATWWR
050200010202     C                   eval      parmbr = vlrhdl
050300010202     C                   movel     'M'           parmbr
050400060113     C                   eval      parccm = vlrksc
050500010202     C                   eval      paropz = '1'
050600010202     C* Effettuo la chiamata al CLLE preposto
050700040506     C                   call(e)   'TITVVTC'
050800010202     C                   parm                    parccm
050900010202     C                   parm                    parmbr
051000010202     C                   parm                    paropz
051100010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
051200010202     C                   if        %error
051300010202     C                   movel     '1'           chkcall
051400010202     C                   else
051500010202     C                   movel     '0'           chkcall
051600010202     C                   endif
051700010202     C*
051800010202     C                   ENDSR
051900000801     C*----------------------------------------------------*
052000000801     C*  CONTROLLO NUMERICITA' CAMPI
052100000801     C*----------------------------------------------------*
052200000801     C     CHKNUM        BEGSR
052300000801     C*
052400000801     C                   call(e)   'ISNUMERIC'
052500000801     C                   PARM                    PiStr            30
052600040714     C                   PARM      ','           PiDecChr          1
052700000801     C                   PARM      *ZEROS        PiVal            30 9
052800000801     C                   PARM      '0'           PiInt             1
052900000801     C                   PARM      '0'           PiNum             1
053000000801     C                   IF        %error
053100000801     C                   EVAL      PiInt=*off
053200000801     C                   ENDIF
053300000801     C*
053400000801     C                   ENDSR
053500000801     C***
053600000801
053700011113
053800011113     C*----------------------------------------------------*
053900011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
054000011113     C*----------------------------------------------------*
054100011113     C     CHKIMPDIV     BEGSR
054200011113     C*
054300011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
054400011113     C                   Z-ADD     *zeros        wrkDec            9 9
054500011113     C*
054600011113     C* Come prima cosa effettuo considerazioni sulla divisa
054700011113     C                   IF        vabIAS > *zeros
054800011113     C                   IF        vabVAS <> 'EUR'
054900011113     C                   EVAL      vabVAS =  'ITL'
055000011113     C                   ENDIF
055100011113     C                   ENDIF
055200011113     C*
055300011113     C                   IF        vabCAS > *zeros
055400011113     C                   IF        vabVCA <> 'EUR'
055500011113     C                   EVAL      vabVCA =  'ITL'
055600011113     C                   ENDIF
055700011113     C                   ENDIF
055800011113     C*
055900011113     C                   IF        vabVMD > *zeros
056000020305     C                   IF        vabVAD <> 'EUR'
056100011113     C                   EVAL      vabVAD =  'ITL'
056200011113     C                   ENDIF
056300011113     C                   ENDIF
056400011113     C*
056500011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
056600011113     C                   Z-ADD     vabIAS        wrkDec
056700011113     C                   IF        wrkDec > *zeros
056800011113     C                   IF        vabVAS = 'ITL'
056900011113     C                   EVAL      vabIAS = *zeros
057000011113     C                   ENDIF
057100011113     C                   ENDIF
057200011113     C*
057300011113     C* Stabilisco se il contrasegno ha decimali valorizzati
057400011113     C                   Z-ADD     vabCAS        wrkDec
057500011113     C                   IF        wrkDec > *zeros
057600011113     C                   IF        vabVCA = 'ITL'
057700011113     C                   EVAL      vabCAS = *zeros
057800011113     C                   ENDIF
057900011113     C                   ENDIF
058000011113     C*
058100011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
058200011113     C                   Z-ADD     vabVMD        wrkDec
058300011113     C                   IF        wrkDec > *zeros
058400011113     C                   IF        vabVAD = 'ITL'
058500011113     C                   EVAL      vabVMD = *zeros
058600011113     C                   ENDIF
058700011113     C                   ENDIF
058800011113     C*
058900011113     C                   ENDSR
059000011113     C***
059100011113
059200011113
059300000801
059400000801
059500990920      /TITLE Invio dei dati al punto operativo.
059600010202     C     invio         BEGSR
059700990920     C*
059800021113     C* 1° invio FIVAT
059900010201     C                   reset                   dscmz
060000010201     C                   move      vlrpoi        cmzdst
060100021113     C                   eval      cmzfld = 'FIVATWWR'
060200010201     C                   eval      cmzmbd = vlrhdl
060300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
060400021009     C***                if        prmfir = *blanks
060500021113     C                   eval      cmzfla = 'FIVAT00F'
060600021113     C                   eval      cmzmba = 'FIVAT00F'
060700021009     C***                else
060800021009     C***                eval      cmzfla = prmfir
060900021009     C***                eval      cmzmba = prmfir
061000021009     C***                endif
061100010201     C                   eval      cmznrr = *zeros
061200020305     C                   move      §ctrokvt      cmznrr
061300021018     C                   eval      cmzlba = vlrfl1
061400010201     C                   call(e)   'TIS711C'
061500010201     C                   parm                    dscmz
061600010201     C                   parm      *blanks       esito
061700010205     C                   if        %error
061800010205     C                             or cmzerr = '1'
061900010205     C                             or esito  = '1'
062000010205     C                   eval      wrkesito = '3'
062100010205     C                   else
062200010201     C*
062300021113     C* 2° invio FIVAB
062400010201     C                   reset                   dscmz
062500010201     C                   move      vlrpoi        cmzdst
062600010201     C                   eval      cmzfld = vlrfou
062700010201     C                   eval      cmzmbd = vlrhdl
062800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
062900021009     C***                if        prmfir = *blanks
063000021113     C                   eval      cmzfla = 'FIVAB00F'
063100021113     C                   eval      cmzmba = 'FIVAB00F'
063200021009     C***                else
063300021009     C***                eval      cmzfla = prmfir
063400021009     C***                eval      cmzmba = prmfir
063500021009     C***                endif
063600010201     C                   eval      cmznrr = *zeros
063700010201     C                   move      §ctrokvb      cmznrr
063800021018     C                   eval      cmzlba = vlrfl1
063900010201     C                   call(e)   'TIS711C'
064000010201     C                   parm                    dscmz
064100010201     C                   parm      *blanks       esito
064200010201     C                   if        %error
064300010201     C                             or cmzerr = '1'
064400010201     C                             or esito  = '1'
064500010201     C                   eval      wrkesito = '3'
064600010201     C                   endif
064700010205     C                   endif
064800990920     C*
064900000613     C                   ENDSR
065000000613     C***
065100070411
065200070411     C     *pssr         BEGSR
065300070411     C*
065400070411     C                   if        %open(tivin00r)
065500070411     C                   close     tivin00r
065600070411     C                   endif
065700070411     C                   if        %open(fivabwwr)
065800070411     C                   close     fivabwwr
065900070411     C                   endif
066000070411     C                   if        %open(fivatwwr)
066100070411     C                   close     fivatwwr
066200070411     C                   endif
066300070411     C*
066400070411     C* Effettuo la chiamata al CLLE preposto
066500070411     C                   call(e)   'TITVVTC'
066600070411     C                   parm                    parccm
066700070411     C                   parm                    parmbr
066800070411     C                   parm      '2'           paropz
066900070411     C*
067000070411     C                   eval      wrkesito = '2'
067100070411     C*
067200070411     C                   seton                                        LR
067300070411     C*
067400070411     C                   ENDSR     '*CANCL'
067500070411     C***
067600070411
067700990910
067800000613     C     *inzsr        BEGSR
067900990910     C*
068000990910     C     *entry        plist
068100990920     C                   parm                    tivlrds
068200990921     C                   parm      wrkesito      esito
068300000724     C                   parm                    prmlit
068400000710     C                   parm                    prmfir
068500000613     C*
068600000830     C* CALCOLA LA DATA CORRENTE
068700000830     C                   time                    wn14             14 0
068800000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
068900000830     C                   z-add     wn8           g08dat
069000000830     C                   z-add     *zeros        g08inv
069100000830     C                   movel     '0'           g08err
069200000830     C                   call      'XSRDA8'
069300000830     C                   parm                    wlbda8
069400000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
069500000830     C*
069600000613     C                   ENDSR
069700000613     C***
069800990908
069900070719     Otitvz7p   E            riepilogo         2
070000990915     O                                              'Upload via Internet'
070100990915     O                                           +1 'Traduzione TIVIN00R -'
070200021113     O                                           +1 'FIVABWWR/FIVATWWR'
070300010201     O                                           +1 'Report testate bolle'
070400990915     O          E            riepilogo   2
070500990915     O                       wrkdata
070600990915     O                       wrkora              +1
070700990915     O                       procname            +1
070800990915     O          E            riepilogo   2
070900990915     O                                              'Cliente..................:'
071000990915     O                       VABCCM        z     +1
071100990915     O          E            riepilogo   2
071200990920     O                                              'Riferimento Strategi.....:'
071300990920     O                       vlrhdl              +1
071400990915     O          E            riepilogo   2
071500990915     O                                              'Giusti...................:'
071600010201     O                       §CTROKVB      2   +  1
071700990915     O          E            riepilogo   2
071800990915     O                                              'Sbagliati e corretti.....:'
071900971022     O                       §CTRMO        2   +  1
072000990915     O          E            riepilogo   2
072100990915     O                                              'Sbagliati e scartati.....:'
072200971022     O                       §CTRNO        2   +  1
072300000613
072400070719     Otitvz7ps  E            testdett          2
072500000613     O                                              'Upload via Internet'
072600000613     O                                           +1 'Traduzione TIVIN00R -'
072700021113     O                                           +1 'FIVABWWR/FIVATWWR'
072800010201     O                                           +1 'Report testate bolle'
072900000616     O          E            testdett    3
073000000613     O                                           +2 'N° rec'
073100000613     O                                           +3 'Anteprima contenuto'
073200000616     O          E            rigadett    2
073300000613     O                       rrnum               +2
073400000621     O                       recko               +3
073500000616     O          E            findett     2
073600000613     O                       wrkdata
073700000613     O                       wrkora              +1
073800000613     O                       procname            +1
073900000616     O          E            findett     2
074000000613     O                                              'Cliente..................:'
074100000613     O                       VABCCM        z     +1
074200000616     O          E            findett     2
074300000613     O                                              'Riferimento Strategi.....:'
074400000613     O                       vlrhdl              +1
074500000616     O          E            findett     2
074600000613     O                                              'Giusti...................:'
074700010201     O                       §CTROKVB      2   +  1
074800000616     O          E            findett     2
074900000613     O                                              'Sbagliati e corretti.....:'
075000000613     O                       §CTRMO        2   +  1
075100000616     O          E            findett     2
075200000613     O                                              'Sbagliati e scartati.....:'
075300000613     O                       §CTRNO        2   +  1
075400000512** CMD - COMANDI CL
075500070719OVRPRTF FILE(TITVZ7P) TOFILE(TIS7T7P) OVRSCOPE(*CALLLVL)   FORMTYPE(RICCLI) OUTQ(
075600070719OVRPRTF FILE(TITVZ7PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
075700070719DLTOVR FILE(TITVZ7P TITVZ7PS) LVL(*)
075800000512
075900000512
