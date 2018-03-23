000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021113     FFIVABwwr  O    E             DISK    usropn
000700021113     FFIVATwwr  O    E             DISK    usropn
000800060529     Ftitvt2p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100060529     Ftitvt2ps  O    f  198        PRINTER usropn
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
002600060529     D tisi95ds      e ds
002700990910     D esito           s              1
002800000724     D prmlit          s             10
002900000710     D prmfir          s             10
003000990921     D wrkesito        s                   like(esito)
003100990915     D wrkdata         s               d
003200990915     D wrkora          s               t
003300000613     D rrnum           s              6  0 INZ(*zeros)
003400000621     D recko           s            150    INZ(*blanks)
003500011113     D depcmd          s            150    INZ(*blanks)
003600010202     D parccm          s              8    INZ(*blanks)
003700010202     D parmbr          s             10    INZ(*blanks)
003800010202     D paropz          s              1    INZ(*blanks)
003900010202     D chkcall         s              1    INZ(*blanks)
004000000830
004100000830     D*------------------
004200000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
004300000830     D*------------------
004400000830     D WLBDA8          DS                  INZ
004500000830     D  G08DAT                 1      8  0
004600000830     D  G08INV                 9     16  0
004700000830     D  G08ERR                17     17
004800000830     D  G08TGI                18     22  0
004900000830     D*
005000990908
005100060307     D*------------------
005200060307     D* DS REPERIMENTO NUMERATORE
005300060307     D*------------------
005400060529     D trul33ds      e ds                  inz
005500060307     D*------------------
005600060307     D* DS ARCHITETTURA
005700060307     D*------------------
005800060307     D kpjba         e ds                  inz
005900060307     D*------------------
006000010201
006100010201
006200990915     C                   time                    wrkdata
006300990915     C                   time                    wrkora
006400000913     C                   reset                   rrnum
006500990921     C                   reset                   esito
006600990921     C                   reset                   wrkesito
006700000724     C*
006800000724     C* SE OCCORRE SPEDIRE IN FILIALE
006900000724     C                   if        vlrpoi <> *zeros
007000000724     C*
007100000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
007200000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007300000724     C     vlrpoi        chain     azorg01l
007400000724     C                   if        %found
007500000616     C                   movel(p)  CMD(1)        depcmd
007600020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
007700000616     C*
007800000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
007900011113     C                   Z-ADD     150           LENGH            15 5
008000000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008100000616     C                   PARM                    depcmd
008200000616     C                   PARM                    LENGH
008300000724     C*
008400000724     C                   endif
008500000724     C                   endif
008600000616     C*
008700000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008800000616     C                   movel(p)  CMD(2)        depcmd
008900000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
009000000616     C*
009100000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
009200011113     C                   Z-ADD     150           LENGH            15 5
009300000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009400000616     C                   PARM                    depcmd
009500000616     C                   PARM                    LENGH
009600000616     C*
009700060529     C                   if        not %open(titvt2ps)
009800060529     C                   open      titvt2ps
009900000616     C                   except    testdett
010000000613     C                   endif
010100000613     C*
010200040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
010300000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
010400000613     C*
010500010202     C* Effettuo la chiamata al CLLE preposto
010600040506     C                   call(e)   'TITVVTC'
010700010202     C                   parm                    parccm
010800010202     C                   parm                    parmbr
010900010202     C                   parm      '2'           paropz
011000050201     C*
011100050201     C* Effettuo lancio TISI95 solo x chiusura
011200050201     C                   CLEAR                   TISI95DS
011300050201     C                   EVAL      I95TLA = 'C'
011400050201     C                   CALL      'TISI95R'
011500050201     C                   PARM                    TISI95DS
011600010202     C*
011700060529     C                   if        %open(titvt2ps)
011800000616     C                   except    findett
011900060529     C                   close     titvt2ps
012000000613     C                   endif
012100000616     C*
012200000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
012300011113     C                   Z-ADD     150           LENGH            15 5
012400000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
012500000616     C                   PARM                    CMD(3)
012600000616     C                   PARM                    LENGH
012700000616     C*
012800000801     C
012900010201     C                   seton                                        LR
013000000613
013100000613
013200000613     C*--------------------------------------------------------
013300000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
013400000613     C*--------------------------------------------------------
013500000621     C     STPR          BEGSR
013600000613     C*
013700060529     C                   if        not %open(titvt2p)
013800060529     C                   open      titvt2p
013900990915     C                   endif
014000990915     C*
014100990915     C                   except    riepilogo
014200990915     C*
014300060529     C                   if        %open(titvt2p)
014400060529     C                   close     titvt2p
014500990914     C                   endif
014600990910     C*
014700000613     C                   ENDSR
014800000613     C***
014900990908
015000000801
015100910830     C*--------------------------------------------------------
015200040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
015300910830     C*--------------------------------------------------------
015400040526     C     RWFILE        BEGSR
015500990910     C*
015600990914     C                   if        not %open(tivin00r)
015700990908     C                   open      tivin00r
015800990914     C                   endif
015900021113     C                   if        not %open(fivabwwr)
016000021113     C                   open      fivabwwr
016100990914     C                   endif
016200021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
016300020305     C                   exsr      prevat
016400010201     C*
016500010202     C                   if        chkcall = '0'
016600010202     C*
016700021113     C                   if        not %open(fivatwwr)
016800021113     C                   open      fivatwwr
016900010201     C                   endif
017000990910     C*
017100010201     C                   clear                   §CTROKVB          5 0
017200020305     C                   clear                   §CTROKVT          5 0
017300000801     C                   clear                   §CTRMO            5 0
017400000801     C                   clear                   §CTRNO            5 0
017500990910     C*
017600921023     C                   DO        *HIVAL
017700990913     C*
017800990915     C                   READ      tivin00r                               70
017900050627     C                   if        vindta > *blanks
018000000613     C                   add       1             rrnum
018100000801     C*
018200000801     C                   if        *in70 = *off
018300000801     C                             and
018400000801     C                             (vinflg = *blanks
018500000801     C                              or vinflg = '0'
018600000801     C                              or vinflg = '2')
018700000801     C*
018800000801     C                   clear                   vinmsg
018900000801     C                   eval      vinflg = '1'
019000050628     C*
019100050628     C                   exsr      impvab                                       => carico VAB
019200040802     C                   exsr      wrivab
019300000905     C*
019400000905     C                   else
019500000905     C                   eval      vinflg = '1'
019600050628     C                   endif
019700000905     C                   endif
019800000905     C*
019900000905     C  N70              update    tivin000
020000000905     C*
020100991022     C  N70              ENDdo
020200010202     C*
020300010202     C                   endif
020400990910
020500990910     C* Se non ci sono record con errori ...
020600000710     C                   if        §ctrno = 0
020700990910     C* ... restituisco esito OK.
020800990921     C                   eval      wrkesito = '0'
020900990910     C                   else
021000010201     C                   if        §ctrokvb > 0
021100990921     C                   eval      wrkesito = '1'
021200000710     C                   else
021300000710     C                   eval      wrkesito = '2'
021400990910     C                   endif
021500000710     C                   endif
021600990910     C*
021700990914     C                   if        %open(tivin00r)
021800990908     C                   close     tivin00r
021900990914     C                   endif
022000021113     C                   if        %open(fivabwwr)
022100021113     C                   close     fivabwwr
022200990914     C                   endif
022300021113     C                   if        %open(fivatwwr)
022400021113     C                   close     fivatwwr
022500010201     C                   endif
022600990910     C*
022700010201     C                   if        §ctrokvb > 0
022800000724     C                             and vlrpoi <> *zeros
022900010202     C                   exsr      invio
023000990920     C                   endif
023100990920     C*
023200910830     C                   ENDSR
023300000613     C***
023400010305
023500010305     C*----------------------------------------------------*
023600020305     C*  SCARICAMENTO BUFFER RECORDS VAB
023700010305     C*----------------------------------------------------*
023800020305     C     WRIVAB        BEGSR
023900010305     C*
024000060225     C* Quindi scarico il buffer del file d testata
024100021113     C                   write     fivab000                                     => scarico il VAB
024200010305     C*
024300010305     C                   ENDSR
024400990920
024500000801     C*----------------------------------------------------*
024600000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
024700000801     C*----------------------------------------------------*
024800010201     C     INZVAR        BEGSR
024900000801     C*
025000040802     C                   Z-ADD     *zeros        Num5_0            5 0
025100040802     C                   MOVEL     '0'           FlgCAS            1
025200000801     C*
025300000801     C                   ENDSR
025400000801     C*----------------------------------------------------*
025500000801     C*  IMPOSTAZIONE CAMPI COSTANTI
025600000801     C*----------------------------------------------------*
025700000801     C     DEFCAM        BEGSR
025800000801     C*
025900020619     C* Imposto i valori di default...
026000060529     C                   Z-ADD     *zeros        VABCCM
026100060529     C                   Z-ADD     *zeros        VATCCM
026200060711     C                   Z-ADD     049           VABLNP
026300060711     C                   Z-ADD     049           VATLNP
026400040714     C                   Z-ADD     000           VABCTR
026500060711     C                   MOVEL     '1'           VABCBO
026600060830     C                   MOVEL     *blanks       FlgCLIENTE        3
026700020619     C* ... e poi verifico se sono stati passati come parametri
026800020619     C                   IF        vlrppt > *blanks
026900040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
027000020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
027100020619     C                   EXSR      CHKNUM
027200020619     C                   IF        PiInt=*on
027300020619     C                   Z-ADD     PiVal         VABCCM
027400020619     C                   Z-ADD     PiVal         VATCCM
027500020619     C                   ENDIF
027600040506     C                   ENDIF
027700040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
027800020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
027900020619     C                   EXSR      CHKNUM
028000020619     C                   IF        PiInt=*on
028100020619     C                   Z-ADD     PiVal         VABLNP
028200020619     C                   Z-ADD     PiVal         VATLNP
028300040506     C                   ENDIF
028400020619     C                   ENDIF
028500040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
028600020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
028700020619     C                   EXSR      CHKNUM
028800020619     C                   IF        PiInt=*on
028900020619     C                   Z-ADD     PiVal         VABCTR
029000040506     C                   ENDIF
029100020619     C                   ENDIF
029200020619     C                   ENDIF
029300000801     C*
029400000801     C                   ENDSR
029500000801     C*----------------------------------------------------*
029600021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
029700000801     C*----------------------------------------------------*
029800040823     C     IMPVAB        BEGSR
029900040823     C*
030000020305     C                   EXSR      INZVAR
030100020305     C                   EXSR      DEFCAM
030200010305     C*
030300000801     C                   Z-ADD     *zeros        errore            1 0
030400000830     C                   MOVEL     datcor        VABAAS
030500020305     C                   MOVEL     datcor        VATAAS
030600040526     C                   MOVE      datcor        VABMGS
030700060529     C                   MOVE(P)   vlrpoi        VABFGS
030800060529     C                   MOVE(P)   vlrpoi        VATFGS
030900060711     C* CCM/LNP/CTR
031000060711     C                   IF        %trim(%subst(vindta:463:4))='LAS'
031100060711     C                   EVAL      VABCCM = 0493376
031200060711     C                   EVAL      VABLNP = 049
031300060711     C                   EVAL      VABFGS = 049
031400060711     C                   EVAL      VABCTR = 001
031500060719     C                   EVAL      vlrpoi = VABFGS
031600060828     C                   EVAL      FlgCLIENTE = 'DYN'
031700060529     C                   ENDIF
031800060711     C                   IF        %trim(%subst(vindta:463:4))='S'
031900060711     C                   EVAL      VABCCM = 0493375
032000060711     C                   EVAL      VABLNP = 049
032100060711     C                   EVAL      VABFGS = 049
032200060711     C                   EVAL      VABCTR = 001
032300060719     C                   EVAL      vlrpoi = VABFGS
032400060828     C                   EVAL      FlgCLIENTE = 'ROS'
032500060711     C                   ENDIF
032600060711     C                   IF        %trim(%subst(vindta:463:4))='PMDD'
032700060711     C                   EVAL      VABCCM = 0493378
032800060711     C                   EVAL      VABLNP = 049
032900060711     C                   EVAL      VABFGS = 049
033000060711     C                   EVAL      VABCTR = 003
033100060719     C                   EVAL      vlrpoi = VABFGS
033200060828     C                   EVAL      FlgCLIENTE = 'ROS'
033300060711     C                   ENDIF
033400060711     C                   IF        %trim(%subst(vindta:463:4))='MB'
033500061115     C                   EVAL      VABCCM = 0593106
033600060711     C                   EVAL      VABLNP = 059
033700060711     C                   EVAL      VABFGS = 059
033800060711     C                   EVAL      VABCTR = 000
033900060719     C                   EVAL      vlrpoi = VABFGS
034000060828     C                   EVAL      FlgCLIENTE = 'ROS'
034100060711     C                   ENDIF
034200060711     C                   IF        %trim(%subst(vindta:463:4))='MBDD'
034300061115     C                   EVAL      VABCCM = 0593107
034400060711     C                   EVAL      VABLNP = 059
034500060711     C                   EVAL      VABFGS = 059
034600060711     C                   EVAL      VABCTR = 000
034700060719     C                   EVAL      vlrpoi = VABFGS
034800060828     C                   EVAL      FlgCLIENTE = 'DYN'
034900060711     C                   ENDIF
035000060828     C*
035100060828     C                   EVAL      VABRSD=%trim(%subst(vindta:32:35))
035200060828     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
035300060828     C     '@':'A'       XLATE     VABRSD        VABRSD
035400060828     C* ==
035500060828     C                   IF        FlgCLIENTE = 'DYN'
035600060828     C                   EVAL      VABRD2=%trim(%subst(vindta:67:35))
035700060828     C                   EVAL      VABIND=%trim(%subst(vindta:102:70))
035800060828     C                   ENDIF
035900060828     C                   IF        FlgCLIENTE = 'ROS'
036000060828     C                   EVAL      VABIND=%trim(%subst(vindta:67:105))
036100060828     C                   ENDIF
036200060828     C                   EVAL      VABLOD=%trim(%subst(vindta:177:50))
036300060828     C                   EVAL      VABNZD=%trim(%subst(vindta:487:2))
036400060828     C                   IF        VABNZD='IT' or VABNZD = 'ITA'
036500060828     C                   EVAL      VABNZD=*blanks
036600060828     C                   ENDIF
036700060828     C                   EVAL      VABRMA=%trim(%subst(vindta:3:7))
036800060828     C                   EVAL      VABNOT=%trim(%subst(vindta:367:35))
036900060828     C                   EVAL      VABNT2=%trim(%subst(vindta:367+35:75-35))
037000060529     C* RMN
037100060711     C                   EVAL      PiStr=%trim(%subst(vindta:3:7))
037200060529     C                   EXSR      CHKNUM
037300060529     C                   IF        PiInt=*on
037400060529     C                   Z-ADD     PiVal         VABRMN
037500060529     C                   ELSE
037600060529     C                   ADD       1             errore
037700060529     C                   EVAL      vinmsg = %trimr(vinmsg)
037800060529     C                             + ' ' + 'VABRMN'
037900060529     C                   ENDIF
038000060529     C* NSP => Stacco un numeratore da AZNUM
038100060529     C                   clear                   TRUL33DS
038200060529     C                   eval      I33OPE = *zeros
038300060529     C                   eval      I33CNU = 302
038400060529     C                   eval      I33NUM = 1
038500060529     C                   movel     TRUL33DS      KPJBU
038600060529     C                   call      'TRUL33R'
038700060529     C                   parm                    KPJBA
038800060529     C                   movel     KPJBU         TRUL33DS
038900060529     C                   if        O33ERR = *zeros
039000060529     C                   z-add     O33NRF        VABNSP
039100060529     C                   z-add     O33NRF        VATNSP
039200060529     C                   else
039300060529     C                   Z-ADD     1             errore
039400060529     C                   EVAL      vinmsg = %trimr(vinmsg)
039500060529     C                             + ' ' + 'VABNSP VATNSP'
039600060529     C                   endif
039700060529     C* CAD
039800060711     C                   EVAL      PiStr=%trim(%subst(vindta:172:5))
039900010201     C                   EXSR      CHKNUM
040000010201     C                   IF        PiInt=*on
040100010201     C                   Z-ADD     PiVal         Num5_0
040200040506     C                   MOVEL(P)  Num5_0        VABCAD
040300010201     C                   ELSE
040400040506     C                   ADD       1             errore
040500010201     C                   EVAL      vinmsg = %trimr(vinmsg)
040600040506     C                             + ' ' + 'VABCAD'
040700010201     C                   ENDIF
040800040506     C* Reperisco la provincia dal CAP e dalla località
040900040526     C                   IF        VABCAD <> *blanks AND
041000040526     C                             VABPRD  = *blanks
041100040506     C                   CLEAR                   TISI95DS
041200040506     C                   EVAL      I95TCN = '3'
041300040506     C                   Z-ADD     datcor        I95DAT
041400040506     C                   EVAL      I95CAP = VABCAD
041500040506     C                   EVAL      I95LOC = VABLOD
041600050627     C                   EVAL      I95NAR = VABNZD
041700040506     C                   CALL      'TISI95R'
041800040506     C                   PARM                    TISI95DS
041900040506     C                   EVAL      VABPRD = O95PRV
042000040506     C                   ENDIF
042100060711     C* NCL
042200060711     C                   EVAL      PiStr=%trim(%subst(vindta:449:4))
042300060711     C                   EXSR      CHKNUM
042400060711     C                   IF        PiInt=*on
042500060711     C                   ADD       PiVal         VABNCL
042600060711     C                   ELSE
042700060711     C                   ADD       1             errore
042800060711     C                   EVAL      vinmsg = %trimr(vinmsg)
042900060711     C                             + ' ' + 'VABNCL'
043000060711     C                   ENDIF
043100040506     C* PKB
043200060711     C                   EVAL      PiStr=%trim(%subst(vindta:453:10))
043300010201     C                   EXSR      CHKNUM
043400010201     C                   IF        PiNum=*on
043500060529     C                   EVAL      Pival = Pival/1000                           * gestisco 3 decim.
043600060529     C                   ADD(H)    PiVal         VABPKB
043700010201     C                   ELSE
043800010201     C                   ADD       1             errore
043900010201     C                   EVAL      vinmsg = %trimr(vinmsg)
044000010201     C                             + ' ' + 'VABPKB'
044100010201     C                   ENDIF
044200060711     C*
044300060711     C* Gestione estensione testata bolle FNVAT
044400060711     C                   EXSR      EXEVAT
044500010205     C*
044600010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
044700040802     C                   IF        FlgCAS <> '0'
044800010205     C                   EVAL      VABCBO = '4'
044900010205     C                   ELSE
045000010205     C                   EVAL      VABCBO = '1'
045100010205     C                   ENDIF
045200020305     C*
045300011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
045400011113     C                   EXSR      CHKIMPDIV
045500010202     C*
045600000801     C* Ebbene...
045700000801     C                   ADD       1             §CTRMO
045800010201     C                   IF        errore <> *zeros
045900000801     C                   ADD       1             §CTRNO
046000000801     C                   EVAL      recko = vindta
046100000801     C                   EXCEPT    rigadett
046200000801     C                   EVAL      vinflg = '2'
046300000801     C                   ELSE
046400010201     C                   ADD       1             §CTROKVB
046500000801     C                   ENDIF
046600000801     C*
046700000801     C                   ENDSR
046800050628     C*----------------------------------------------------*
046900060711     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
047000050628     C*----------------------------------------------------*
047100060711     C     EXEVAT        BEGSR
047200050628     C*
047300060711     C* Valorizzo il referente consegna
047400060711     C                   EVAL      VATTRC='A'
047500060711     C                   EVAL      VATNOT=%trim(%subst(vindta:257:50))
047600050628     C                   exsr      wrivat                                       => scarico VAT
047700060711     C*
047800060711     C* Valorizzo il telefono consegna
047900060711     C                   EVAL      VATTRC='B'
048000060711     C                   EVAL      VATNOT=%trim(%subst(vindta:307:15))
048100060711     C                   exsr      wrivat                                       => scarico VAT
048200050628     C*
048300050628     C                   ENDSR
048400010201     C*----------------------------------------------------*
048500040802     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
048600010201     C*----------------------------------------------------*
048700020305     C     WRIVAT        BEGSR
048800060711     C*
048900060711     C                   EVAL      VATAAS = VABAAS
049000060711     C                   EVAL      VATCCM = VABCCM
049100060711     C                   EVAL      VATLNP = VABLNP
049200060711     C                   EVAL      VATNSP = VABNSP
049300050628     C*
049400060223     C* Scrivo solo se valorizzato qualcosa
049500060223     C                   IF        VATNOT <> *blanks
049600040802     C                   WRITE     FIVAT000
049700060223     C                   ENDIF
049800010201     C*
049900010201     C                   ENDSR
050000010202     C*----------------------------------------------------*
050100021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
050200010202     C*----------------------------------------------------*
050300020305     C     PREVAT        BEGSR
050400010202     C*
050500021113     C* Compongo il nome del membro da dare al FIVATWWR
050600010202     C                   eval      parmbr = vlrhdl
050700010202     C                   movel     'M'           parmbr
050800050627     C                   eval      parccm = %subst(vlrKSC:2:7)
050900010202     C                   eval      paropz = '1'
051000010202     C* Effettuo la chiamata al CLLE preposto
051100040506     C                   call(e)   'TITVVTC'
051200010202     C                   parm                    parccm
051300010202     C                   parm                    parmbr
051400010202     C                   parm                    paropz
051500010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
051600010202     C                   if        %error
051700010202     C                   movel     '1'           chkcall
051800010202     C                   else
051900010202     C                   movel     '0'           chkcall
052000010202     C                   endif
052100010202     C*
052200010202     C                   ENDSR
052300000801     C*----------------------------------------------------*
052400000801     C*  CONTROLLO NUMERICITA' CAMPI
052500000801     C*----------------------------------------------------*
052600000801     C     CHKNUM        BEGSR
052700000801     C*
052800000801     C                   call(e)   'ISNUMERIC'
052900000801     C                   PARM                    PiStr            30
053000050627     C                   PARM      ','           PiDecChr          1
053100000801     C                   PARM      *ZEROS        PiVal            30 9
053200000801     C                   PARM      '0'           PiInt             1
053300000801     C                   PARM      '0'           PiNum             1
053400000801     C                   IF        %error
053500000801     C                   EVAL      PiInt=*off
053600000801     C                   ENDIF
053700000801     C*
053800000801     C                   ENDSR
053900000801     C***
054000000801
054100011113
054200011113     C*----------------------------------------------------*
054300011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
054400011113     C*----------------------------------------------------*
054500011113     C     CHKIMPDIV     BEGSR
054600011113     C*
054700011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
054800011113     C                   Z-ADD     *zeros        wrkDec            9 9
054900011113     C*
055000011113     C* Come prima cosa effettuo considerazioni sulla divisa
055100011113     C                   IF        vabIAS > *zeros
055200011113     C                   IF        vabVAS <> 'EUR'
055300011113     C                   EVAL      vabVAS =  'ITL'
055400011113     C                   ENDIF
055500011113     C                   ENDIF
055600011113     C*
055700011113     C                   IF        vabCAS > *zeros
055800011113     C                   IF        vabVCA <> 'EUR'
055900011113     C                   EVAL      vabVCA =  'ITL'
056000011113     C                   ENDIF
056100011113     C                   ENDIF
056200011113     C*
056300011113     C                   IF        vabVMD > *zeros
056400020305     C                   IF        vabVAD <> 'EUR'
056500011113     C                   EVAL      vabVAD =  'ITL'
056600011113     C                   ENDIF
056700011113     C                   ENDIF
056800011113     C*
056900011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
057000011113     C                   Z-ADD     vabIAS        wrkDec
057100011113     C                   IF        wrkDec > *zeros
057200011113     C                   IF        vabVAS = 'ITL'
057300011113     C                   EVAL      vabIAS = *zeros
057400011113     C                   ENDIF
057500011113     C                   ENDIF
057600011113     C*
057700011113     C* Stabilisco se il contrasegno ha decimali valorizzati
057800011113     C                   Z-ADD     vabCAS        wrkDec
057900011113     C                   IF        wrkDec > *zeros
058000011113     C                   IF        vabVCA = 'ITL'
058100011113     C                   EVAL      vabCAS = *zeros
058200011113     C                   ENDIF
058300011113     C                   ENDIF
058400011113     C*
058500011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
058600011113     C                   Z-ADD     vabVMD        wrkDec
058700011113     C                   IF        wrkDec > *zeros
058800011113     C                   IF        vabVAD = 'ITL'
058900011113     C                   EVAL      vabVMD = *zeros
059000011113     C                   ENDIF
059100011113     C                   ENDIF
059200011113     C*
059300011113     C                   ENDSR
059400011113     C***
059500011113
059600011113
059700000801
059800000801
059900990920      /TITLE Invio dei dati al punto operativo.
060000010202     C     invio         BEGSR
060100990920     C*
060200021113     C* 1° invio FIVAT
060300010201     C                   reset                   dscmz
060400010201     C                   move      vlrpoi        cmzdst
060500021113     C                   eval      cmzfld = 'FIVATWWR'
060600010201     C                   eval      cmzmbd = vlrhdl
060700010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
060800021009     C***                if        prmfir = *blanks
060900021113     C                   eval      cmzfla = 'FIVAT00F'
061000021113     C                   eval      cmzmba = 'FIVAT00F'
061100021009     C***                else
061200021009     C***                eval      cmzfla = prmfir
061300021009     C***                eval      cmzmba = prmfir
061400021009     C***                endif
061500010201     C                   eval      cmznrr = *zeros
061600020305     C                   move      §ctrokvt      cmznrr
061700021018     C                   eval      cmzlba = vlrfl1
061800010201     C                   call(e)   'TIS711C'
061900010201     C                   parm                    dscmz
062000010201     C                   parm      *blanks       esito
062100010205     C                   if        %error
062200010205     C                             or cmzerr = '1'
062300010205     C                             or esito  = '1'
062400010205     C                   eval      wrkesito = '3'
062500010205     C                   else
062600010201     C*
062700021113     C* 2° invio FIVAB
062800010201     C                   reset                   dscmz
062900010201     C                   move      vlrpoi        cmzdst
063000010201     C                   eval      cmzfld = vlrfou
063100010201     C                   eval      cmzmbd = vlrhdl
063200010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
063300021009     C***                if        prmfir = *blanks
063400021113     C                   eval      cmzfla = 'FIVAB00F'
063500021113     C                   eval      cmzmba = 'FIVAB00F'
063600021009     C***                else
063700021009     C***                eval      cmzfla = prmfir
063800021009     C***                eval      cmzmba = prmfir
063900021009     C***                endif
064000010201     C                   eval      cmznrr = *zeros
064100010201     C                   move      §ctrokvb      cmznrr
064200021018     C                   eval      cmzlba = vlrfl1
064300010201     C                   call(e)   'TIS711C'
064400010201     C                   parm                    dscmz
064500010201     C                   parm      *blanks       esito
064600010201     C                   if        %error
064700010201     C                             or cmzerr = '1'
064800010201     C                             or esito  = '1'
064900010201     C                   eval      wrkesito = '3'
065000010201     C                   endif
065100010205     C                   endif
065200990920     C*
065300000613     C                   ENDSR
065400000613     C***
065500070411
065600070411     C     *pssr         BEGSR
065700070411     C*
065800070411     C                   if        %open(tivin00r)
065900070411     C                   close     tivin00r
066000070411     C                   endif
066100070411     C                   if        %open(fivabwwr)
066200070411     C                   close     fivabwwr
066300070411     C                   endif
066400070411     C                   if        %open(fivatwwr)
066500070411     C                   close     fivatwwr
066600070411     C                   endif
066700070411     C*
066800070411     C* Effettuo la chiamata al CLLE preposto
066900070411     C                   call(e)   'TITVVTC'
067000070411     C                   parm                    parccm
067100070411     C                   parm                    parmbr
067200070411     C                   parm      '2'           paropz
067300070411     C*
067400070411     C                   eval      wrkesito = '2'
067500070411     C*
067600070411     C                   seton                                        LR
067700070411     C*
067800070411     C                   ENDSR     '*CANCL'
067900070411     C***
068000070411
068100990910
068200000613     C     *inzsr        BEGSR
068300990910     C*
068400990910     C     *entry        plist
068500990920     C                   parm                    tivlrds
068600990921     C                   parm      wrkesito      esito
068700000724     C                   parm                    prmlit
068800000710     C                   parm                    prmfir
068900000613     C*
069000000830     C* CALCOLA LA DATA CORRENTE
069100000830     C                   time                    wn14             14 0
069200000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
069300000830     C                   z-add     wn8           g08dat
069400000830     C                   z-add     *zeros        g08inv
069500000830     C                   movel     '0'           g08err
069600000830     C                   call      'XSRDA8'
069700000830     C                   parm                    wlbda8
069800000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
069900000830     C*
070000000613     C                   ENDSR
070100000613     C***
070200990908
070300060529     Otitvt2p   E            riepilogo         2
070400990915     O                                              'Upload via Internet'
070500990915     O                                           +1 'Traduzione TIVIN00R -'
070600021113     O                                           +1 'FIVABWWR/FIVATWWR'
070700010201     O                                           +1 'Report testate bolle'
070800990915     O          E            riepilogo   2
070900990915     O                       wrkdata
071000990915     O                       wrkora              +1
071100990915     O                       procname            +1
071200990915     O          E            riepilogo   2
071300990915     O                                              'Cliente..................:'
071400990915     O                       VABCCM        z     +1
071500990915     O          E            riepilogo   2
071600990920     O                                              'Riferimento Strategi.....:'
071700990920     O                       vlrhdl              +1
071800990915     O          E            riepilogo   2
071900990915     O                                              'Giusti...................:'
072000010201     O                       §CTROKVB      2   +  1
072100990915     O          E            riepilogo   2
072200990915     O                                              'Sbagliati e corretti.....:'
072300971022     O                       §CTRMO        2   +  1
072400990915     O          E            riepilogo   2
072500990915     O                                              'Sbagliati e scartati.....:'
072600971022     O                       §CTRNO        2   +  1
072700000613
072800060529     Otitvt2ps  E            testdett          2
072900000613     O                                              'Upload via Internet'
073000000613     O                                           +1 'Traduzione TIVIN00R -'
073100021113     O                                           +1 'FIVABWWR/FIVATWWR'
073200010201     O                                           +1 'Report testate bolle'
073300000616     O          E            testdett    3
073400000613     O                                           +2 'N° rec'
073500000613     O                                           +3 'Anteprima contenuto'
073600000616     O          E            rigadett    2
073700000613     O                       rrnum               +2
073800000621     O                       recko               +3
073900000616     O          E            findett     2
074000000613     O                       wrkdata
074100000613     O                       wrkora              +1
074200000613     O                       procname            +1
074300000616     O          E            findett     2
074400000613     O                                              'Cliente..................:'
074500000613     O                       VABCCM        z     +1
074600000616     O          E            findett     2
074700000613     O                                              'Riferimento Strategi.....:'
074800000613     O                       vlrhdl              +1
074900000616     O          E            findett     2
075000000613     O                                              'Giusti...................:'
075100010201     O                       §CTROKVB      2   +  1
075200000616     O          E            findett     2
075300000613     O                                              'Sbagliati e corretti.....:'
075400000613     O                       §CTRMO        2   +  1
075500000616     O          E            findett     2
075600000613     O                                              'Sbagliati e scartati.....:'
075700000613     O                       §CTRNO        2   +  1
075800000512** CMD - COMANDI CL
075900060529OVRPRTF FILE(TITVT2P) TOFILE(TIS7T7P) OVRSCOPE(*CALLLVL)   FORMTYPE(RICCLI) OUTQ(
076000060529OVRPRTF FILE(TITVT2PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
076100060529DLTOVR FILE(TITVT2P TITVT2PS) LVL(*)
076200000512
076300000512
