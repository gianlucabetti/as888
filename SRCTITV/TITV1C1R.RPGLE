000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021113     FFIVABwwr  O    E             DISK    usropn
000700021113     FFIVATwwr  O    E             DISK    usropn
000800071030     Ftitv1c1p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100071030     Ftitv1c1ps O    f  198        PRINTER usropn
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
009500071030     C                   if        not %open(titv1c1ps)
009600071030     C                   open      titv1c1ps
009700000616     C                   except    testdett
009800000613     C                   endif
009900000613     C*
010000040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
010100000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
010200000613     C*
010300010202     C* Effettuo la chiamata al CLLE preposto
010400040506     C                   call(e)   'TITVVTC'
010500010202     C                   parm                    parccm
010600010202     C                   parm                    parmbr
010700010202     C                   parm      '2'           paropz
010800010202     C*
010900071030     C                   if        %open(titv1c1ps)
011000000616     C                   except    findett
011100071030     C                   close     titv1c1ps
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
012800071030     C                   if        not %open(titv1c1p)
012900071030     C                   open      titv1c1p
013000990915     C                   endif
013100990915     C*
013200990915     C                   except    riepilogo
013300990915     C*
013400071030     C                   if        %open(titv1c1p)
013500071030     C                   close     titv1c1p
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
024400071030     C                   Z-ADD     0054423       VABCCM
024500071030     C                   Z-ADD     0054423       VATCCM
024600071030     C                   Z-ADD     005           VABLNP
024700071030     C                   Z-ADD     005           VATLNP
024800070531     C                   Z-ADD     000           VABCTR
024900070531     C                   MOVEL     '7Q'          VABCTM
025000040823     C                   MOVEL     '1'           VABCBO
025100020619     C* ... e poi verifico se sono stati passati come parametri
025200020619     C                   IF        vlrppt > *blanks
025300040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
025400020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
025500020619     C                   EXSR      CHKNUM
025600020619     C                   IF        PiInt=*on
025700020619     C                   Z-ADD     PiVal         VABCCM
025800020619     C                   Z-ADD     PiVal         VATCCM
025900020619     C                   ENDIF
026000040506     C                   ENDIF
026100040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
026200020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
026300020619     C                   EXSR      CHKNUM
026400020619     C                   IF        PiInt=*on
026500020619     C                   Z-ADD     PiVal         VABLNP
026600020619     C                   Z-ADD     PiVal         VATLNP
026700040506     C                   ENDIF
026800020619     C                   ENDIF
026900040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
027000020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
027100020619     C                   EXSR      CHKNUM
027200020619     C                   IF        PiInt=*on
027300020619     C                   Z-ADD     PiVal         VABCTR
027400040506     C                   ENDIF
027500020619     C                   ENDIF
027600060202     C                   IF        %subst(vlrppt:14:2) <> *blanks
027700060202     C                   EVAL      VABCTM=%trim(%subst(vlrppt:14:2))
027800060202     C                   ENDIF
027900100316     C                   IF        %subst(vlrppt:16:2) <> *blanks
028000100316     C                   EVAL      VABTIC=%trim(%subst(vlrppt:16:2))
028100100316     C                   ENDIF
028200020619     C                   ENDIF
028300000801     C*
028400000801     C                   ENDSR
028500000801     C*----------------------------------------------------*
028600040910     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB e FIVAT)
028700000801     C*----------------------------------------------------*
028800040910     C     IMPVABVAT     BEGSR
028900040910     C*
029000040910     C* Traduzione relativa ai tipi record del file del cliente
029100040910     C*
029200040910     C***
029300060202     C* ...tipo record 'TS'
029400060202     C                   IF        %subst(vindta:1:2) = 'TS'
029500040910     C* ......inizializzazioni iniziali e formati record file Bartolini
029600040910     C                   EXSR      INZVAR
029700040910     C                   EXSR      DEFCAM
029800040928     C*
029900040928     C                   Z-ADD     *zeros        errore            1 0
030000040928     C* ......valorizzazione campi da data traduzione e da P.O. d gestione
030100040928     C                   MOVEL     datcor        VABAAS
030200040928     C                   MOVEL     datcor        VATAAS
030300040928     C                   MOVE      datcor        VABMGS
030400040928     C                   MOVE(P)   vlrpoi        VABFGS
030500040928     C                   MOVE(P)   vlrpoi        VATFGS
030600040910     C* ......VABNSP/VATNSP
030700041025     C* NSP => Stacco un numeratore da AZNUM
030800041025     C                   clear                   TRUL33DS
030900041025     C                   eval      I33OPE = *zeros
031000041025     C                   eval      I33CNU = 302
031100041025     C                   eval      I33NUM = 1
031200041025     C                   movel     TRUL33DS      KPJBU
031300041025     C                   call      'TRUL33R'
031400041025     C                   parm                    KPJBA
031500041025     C                   movel     KPJBU         TRUL33DS
031600041025     C                   if        O33ERR = *zeros
031700041025     C                   z-add     O33NRF        VABNSP
031800041025     C                   z-add     O33NRF        VATNSP
031900041025     C                   else
032000041025     C                   Z-ADD     1             errore
032100041025     C                   EVAL      vinmsg = %trimr(vinmsg)
032200041025     C                             + ' ' + 'VABNSP VATNSP'
032300041025     C                   endif
032400060202     C* ......VABRMA
032500071030     C                   EVAL      VABRMA=%trim(%subst(vindta:3:10))
032600060202     C* ......VABRMN
032700071030     C                   EVAL      PiStr=%trim(%subst(vindta:4:9))
032800060202     C                   EXSR      CHKNUM
032900060202     C                   IF        PiInt=*on
033000060202     C                   Z-ADD     PiVal         VABRMN
033100060202     C                   ELSE
033200060202     C                   ADD       1             errore
033300060202     C                   EVAL      vinmsg = %trimr(vinmsg)
033400060202     C                             + ' ' + 'VABRMN'
033500060202     C                   ENDIF
033600060202     C* ......VABNCL
033700060202     C                   EVAL      PiStr=%trim(%subst(vindta:59:15))
033800060202     C                   EXSR      CHKNUM
033900060202     C                   IF        PiInt=*on
034000060202     C                   Z-ADD     PiVal         VABNCL
034100060202     C                   ELSE
034200060202     C                   ADD       1             errore
034300060202     C                   EVAL      vinmsg = %trimr(vinmsg)
034400060202     C                             + ' ' + 'VABNCL'
034500060202     C                   ENDIF
034600060202     C* ......VABPKB
034700060202     C                   EVAL      PiStr=%trim(%subst(vindta:74:15))
034800060202     C                   EXSR      CHKNUM
034900060202     C                   IF        PiNum=*on
035000070108     C                   EVAL      PiVal=PiVal/1000                             * gestisco 3 dec.
035100070108     C                   Z-ADD(H)  PiVal         VABPKB
035200060202     C                   ELSE
035300060202     C                   ADD       1             errore
035400060202     C                   EVAL      vinmsg = %trimr(vinmsg)
035500060202     C                             + ' ' + 'VABPKB'
035600060202     C                   ENDIF
035700100324     C                   IF        VABPKB = *zeros
035800100324     C                   EVAL      VABPKB = 0,5
035900100324     C                   ENDIF
036000060202     C* ......VABRSD
036100060202     C                   EVAL      VABRSD=%trim(%subst(vindta:104:35))
036200060202     C* ......VABIND
036300060202     C                   EVAL      VABIND=%trim(%subst(vindta:139:35))
036400060202     C* ......VABLOD
036500060202     C                   EVAL      VABLOD=%trim(%subst(vindta:174:35))
036600060202     C* ......VABCAD
036700060202     C                   EVAL      PiStr=%trim(%subst(vindta:209:6))
036800060202     C                   EXSR      CHKNUM
036900060202     C                   IF        PiInt=*on
037000060202     C                   Z-ADD     PiVal         Num5_0
037100060202     C                   MOVEL(P)  Num5_0        VABCAD
037200060202     C                   ELSE
037300060202     C                   ADD       1             errore
037400060202     C                   EVAL      vinmsg = %trimr(vinmsg)
037500060202     C                             + ' ' + 'VABCAD'
037600060202     C                   ENDIF
037700060202     C* ......VABPRD
037800060202     C                   EVAL      VABPRD=%trim(%subst(vindta:215:2))
037900060202     C* ......VABCAS
038000060202     C                   IF        %subst(vindta:217:15) <> *zeros
038100060202     C                   EVAL      FlgCAS = '1'
038200060202     C                   EVAL      PiStr=%trim(%subst(vindta:217:15))
038300060202     C                   EXSR      CHKNUM
038400060202     C                   IF        PiNum=*on
038500060202     C                   EVAL      VABCAS=PiVal/1000                            * gestisco 3 dec.
038600060202     C                   ELSE
038700060202     C                   ADD       1             errore
038800060202     C                   EVAL      vinmsg = %trimr(vinmsg)
038900060202     C                             + ' ' + 'VABCAS'
039000060202     C                   ENDIF
039100060202     C                   ENDIF
039200060202     C* ......VABVCA
039300060202     C                   EVAL      VABVCA=%trim(%subst(vindta:232:3))
039400100323     C                   IF        VABVCA = *blanks
039500100323     C                   EVAL      VABVCA = 'EUR'
039600100323     C                   ENDIF
039700060202     C* ......VABNOT
039800060202     C                   EVAL      VABNOT=%trim(%subst(vindta:235:30))
039900060202     C* ......VABNT2
040000060202     C                   EVAL      VABNT2=%trim(%subst(vindta:265:30))
040100060202     C* ......VABRMO
040200060202     C                   EVAL      VABRMO=%trim(%subst(vindta:295:30))
040300060202     C* ......VABDCR
040400060202     C                   IF        %subst(vindta:325:8) <> *zeros
040500060202     C                   EVAL      PiStr=%trim(%subst(vindta:325:8))
040600060202     C                   EXSR      CHKNUM
040700060202     C                   IF        PiInt=*on
040800060202     C                   Z-ADD     PiVal         VABDCR
040900060202     C                   ELSE
041000060202     C                   ADD       1             errore
041100060202     C                   EVAL      vinmsg = %trimr(vinmsg)
041200060202     C                             + ' ' + 'VABDCR'
041300060202     C                   ENDIF
041400060202     C                   ENDIF
041500060202     C*
041600060202     C* Considerazioni sul contenuto di campi precedentemente valorizzati
041700060202     C                   IF        FlgCAS <> '0'
041800060202     C                   IF        VABCBO = '1'
041900060202     C                   EVAL      VABCBO = '4'
042000060202     C                   ELSE
042100060202     C                   EVAL      VABCBO = '6'
042200060202     C                   ENDIF
042300060202     C                   ENDIF
042400060202     C*
042500060202     C* Eseguo routine finale x considerazioni specifiche su importi/divise
042600060202     C                   EXSR      CHKIMPDIV
042700060202     C* ...e in fine...
042800060202     C                   WRITE     FIVAB000
042900040910     C*
043000040910     C                   ENDIF
043100040910     C***
043200060202     C* ...tipo record 'RS'
043300060202     C                   IF        %subst(vindta:1:2) = 'RS'
043400040910     C* ......VATNOT
043500060202     C                   EVAL      VATNOT=%trim(%subst(vindta:43:20))
043600040910     C                   EVAL      VATTRC = 'E'
043700060202     C* x ogni tipo record 'RS' devo scaricare il buffer del file FIVAT
043800060202     C                   IF        VATNOT <> *blanks
043900040910     C                   WRITE     FIVAT000
044000060202     C                   ENDIF
044100040910     C*
044200040910     C                   ENDIF
044300010202     C*
044400000801     C* Ebbene...
044500000801     C                   ADD       1             §CTRMO
044600010201     C                   IF        errore <> *zeros
044700000801     C                   ADD       1             §CTRNO
044800000801     C                   EVAL      recko = vindta
044900000801     C                   EXCEPT    rigadett
045000000801     C                   EVAL      vinflg = '2'
045100000801     C                   ELSE
045200010201     C                   ADD       1             §CTROKVB
045300000801     C                   ENDIF
045400000801     C*
045500000801     C                   ENDSR
045600010202     C*----------------------------------------------------*
045700021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
045800010202     C*----------------------------------------------------*
045900020305     C     PREVAT        BEGSR
046000010202     C*
046100021113     C* Compongo il nome del membro da dare al FIVATWWR
046200010202     C                   eval      parmbr = vlrhdl
046300010202     C                   movel     'M'           parmbr
046400060113     C                   eval      parccm = vlrksc
046500010202     C                   eval      paropz = '1'
046600010202     C* Effettuo la chiamata al CLLE preposto
046700040506     C                   call(e)   'TITVVTC'
046800010202     C                   parm                    parccm
046900010202     C                   parm                    parmbr
047000010202     C                   parm                    paropz
047100010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
047200010202     C                   if        %error
047300010202     C                   movel     '1'           chkcall
047400010202     C                   else
047500010202     C                   movel     '0'           chkcall
047600010202     C                   endif
047700010202     C*
047800010202     C                   ENDSR
047900000801     C*----------------------------------------------------*
048000000801     C*  CONTROLLO NUMERICITA' CAMPI
048100000801     C*----------------------------------------------------*
048200000801     C     CHKNUM        BEGSR
048300000801     C*
048400000801     C                   call(e)   'ISNUMERIC'
048500000801     C                   PARM                    PiStr            30
048600040714     C                   PARM      ','           PiDecChr          1
048700000801     C                   PARM      *ZEROS        PiVal            30 9
048800000801     C                   PARM      '0'           PiInt             1
048900000801     C                   PARM      '0'           PiNum             1
049000000801     C                   IF        %error
049100000801     C                   EVAL      PiInt=*off
049200000801     C                   ENDIF
049300000801     C*
049400000801     C                   ENDSR
049500000801     C***
049600000801
049700011113
049800011113     C*----------------------------------------------------*
049900011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
050000011113     C*----------------------------------------------------*
050100011113     C     CHKIMPDIV     BEGSR
050200011113     C*
050300011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
050400011113     C                   Z-ADD     *zeros        wrkDec            9 9
050500011113     C*
050600011113     C* Come prima cosa effettuo considerazioni sulla divisa
050700011113     C                   IF        vabIAS > *zeros
050800011113     C                   IF        vabVAS <> 'EUR'
050900011113     C                   EVAL      vabVAS =  'ITL'
051000011113     C                   ENDIF
051100011113     C                   ENDIF
051200011113     C*
051300011113     C                   IF        vabCAS > *zeros
051400011113     C                   IF        vabVCA <> 'EUR'
051500011113     C                   EVAL      vabVCA =  'ITL'
051600011113     C                   ENDIF
051700011113     C                   ENDIF
051800011113     C*
051900011113     C                   IF        vabVMD > *zeros
052000020305     C                   IF        vabVAD <> 'EUR'
052100011113     C                   EVAL      vabVAD =  'ITL'
052200011113     C                   ENDIF
052300011113     C                   ENDIF
052400011113     C*
052500011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
052600011113     C                   Z-ADD     vabIAS        wrkDec
052700011113     C                   IF        wrkDec > *zeros
052800011113     C                   IF        vabVAS = 'ITL'
052900011113     C                   EVAL      vabIAS = *zeros
053000011113     C                   ENDIF
053100011113     C                   ENDIF
053200011113     C*
053300011113     C* Stabilisco se il contrasegno ha decimali valorizzati
053400011113     C                   Z-ADD     vabCAS        wrkDec
053500011113     C                   IF        wrkDec > *zeros
053600011113     C                   IF        vabVCA = 'ITL'
053700011113     C                   EVAL      vabCAS = *zeros
053800011113     C                   ENDIF
053900011113     C                   ENDIF
054000011113     C*
054100011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
054200011113     C                   Z-ADD     vabVMD        wrkDec
054300011113     C                   IF        wrkDec > *zeros
054400011113     C                   IF        vabVAD = 'ITL'
054500011113     C                   EVAL      vabVMD = *zeros
054600011113     C                   ENDIF
054700011113     C                   ENDIF
054800011113     C*
054900011113     C                   ENDSR
055000011113     C***
055100011113
055200011113
055300000801
055400000801
055500990920      /TITLE Invio dei dati al punto operativo.
055600010202     C     invio         BEGSR
055700990920     C*
055800021113     C* 1° invio FIVAT
055900010201     C                   reset                   dscmz
056000010201     C                   move      vlrpoi        cmzdst
056100021113     C                   eval      cmzfld = 'FIVATWWR'
056200010201     C                   eval      cmzmbd = vlrhdl
056300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
056400021009     C***                if        prmfir = *blanks
056500021113     C                   eval      cmzfla = 'FIVAT00F'
056600021113     C                   eval      cmzmba = 'FIVAT00F'
056700021009     C***                else
056800021009     C***                eval      cmzfla = prmfir
056900021009     C***                eval      cmzmba = prmfir
057000021009     C***                endif
057100010201     C                   eval      cmznrr = *zeros
057200020305     C                   move      §ctrokvt      cmznrr
057300021018     C                   eval      cmzlba = vlrfl1
057400010201     C                   call(e)   'TIS711C'
057500010201     C                   parm                    dscmz
057600010201     C                   parm      *blanks       esito
057700010205     C                   if        %error
057800010205     C                             or cmzerr = '1'
057900010205     C                             or esito  = '1'
058000010205     C                   eval      wrkesito = '3'
058100010205     C                   else
058200010201     C*
058300021113     C* 2° invio FIVAB
058400010201     C                   reset                   dscmz
058500010201     C                   move      vlrpoi        cmzdst
058600010201     C                   eval      cmzfld = vlrfou
058700010201     C                   eval      cmzmbd = vlrhdl
058800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
058900021009     C***                if        prmfir = *blanks
059000021113     C                   eval      cmzfla = 'FIVAB00F'
059100021113     C                   eval      cmzmba = 'FIVAB00F'
059200021009     C***                else
059300021009     C***                eval      cmzfla = prmfir
059400021009     C***                eval      cmzmba = prmfir
059500021009     C***                endif
059600010201     C                   eval      cmznrr = *zeros
059700010201     C                   move      §ctrokvb      cmznrr
059800021018     C                   eval      cmzlba = vlrfl1
059900010201     C                   call(e)   'TIS711C'
060000010201     C                   parm                    dscmz
060100010201     C                   parm      *blanks       esito
060200010201     C                   if        %error
060300010201     C                             or cmzerr = '1'
060400010201     C                             or esito  = '1'
060500010201     C                   eval      wrkesito = '3'
060600010201     C                   endif
060700010205     C                   endif
060800990920     C*
060900000613     C                   ENDSR
061000000613     C***
061100070411
061200070411     C     *pssr         BEGSR
061300070411     C*
061400070411     C                   if        %open(tivin00r)
061500070411     C                   close     tivin00r
061600070411     C                   endif
061700070411     C                   if        %open(fivabwwr)
061800070411     C                   close     fivabwwr
061900070411     C                   endif
062000070411     C                   if        %open(fivatwwr)
062100070411     C                   close     fivatwwr
062200070411     C                   endif
062300070411     C*
062400070411     C* Effettuo la chiamata al CLLE preposto
062500070411     C                   call(e)   'TITVVTC'
062600070411     C                   parm                    parccm
062700070411     C                   parm                    parmbr
062800070411     C                   parm      '2'           paropz
062900070411     C*
063000070411     C                   eval      wrkesito = '2'
063100070411     C*
063200070411     C                   seton                                        LR
063300070411     C*
063400070411     C                   ENDSR     '*CANCL'
063500070411     C***
063600070411
063700990910
063800000613     C     *inzsr        BEGSR
063900990910     C*
064000990910     C     *entry        plist
064100990920     C                   parm                    tivlrds
064200990921     C                   parm      wrkesito      esito
064300000724     C                   parm                    prmlit
064400000710     C                   parm                    prmfir
064500000613     C*
064600000830     C* CALCOLA LA DATA CORRENTE
064700000830     C                   time                    wn14             14 0
064800000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
064900000830     C                   z-add     wn8           g08dat
065000000830     C                   z-add     *zeros        g08inv
065100000830     C                   movel     '0'           g08err
065200000830     C                   call      'XSRDA8'
065300000830     C                   parm                    wlbda8
065400000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
065500000830     C*
065600000613     C                   ENDSR
065700000613     C***
065800990908
065900071030     Otitv1c1p  E            riepilogo         2
066000990915     O                                              'Upload via Internet'
066100990915     O                                           +1 'Traduzione TIVIN00R -'
066200021113     O                                           +1 'FIVABWWR/FIVATWWR'
066300010201     O                                           +1 'Report testate bolle'
066400990915     O          E            riepilogo   2
066500990915     O                       wrkdata
066600990915     O                       wrkora              +1
066700990915     O                       procname            +1
066800990915     O          E            riepilogo   2
066900990915     O                                              'Cliente..................:'
067000990915     O                       VABCCM        z     +1
067100990915     O          E            riepilogo   2
067200990920     O                                              'Riferimento Strategi.....:'
067300990920     O                       vlrhdl              +1
067400990915     O          E            riepilogo   2
067500990915     O                                              'Giusti...................:'
067600010201     O                       §CTROKVB      2   +  1
067700990915     O          E            riepilogo   2
067800990915     O                                              'Sbagliati e corretti.....:'
067900971022     O                       §CTRMO        2   +  1
068000990915     O          E            riepilogo   2
068100990915     O                                              'Sbagliati e scartati.....:'
068200971022     O                       §CTRNO        2   +  1
068300000613
068400071030     Otitv1c1ps E            testdett          2
068500000613     O                                              'Upload via Internet'
068600000613     O                                           +1 'Traduzione TIVIN00R -'
068700021113     O                                           +1 'FIVABWWR/FIVATWWR'
068800010201     O                                           +1 'Report testate bolle'
068900000616     O          E            testdett    3
069000000613     O                                           +2 'N° rec'
069100000613     O                                           +3 'Anteprima contenuto'
069200000616     O          E            rigadett    2
069300000613     O                       rrnum               +2
069400000621     O                       recko               +3
069500000616     O          E            findett     2
069600000613     O                       wrkdata
069700000613     O                       wrkora              +1
069800000613     O                       procname            +1
069900000616     O          E            findett     2
070000000613     O                                              'Cliente..................:'
070100000613     O                       VABCCM        z     +1
070200000616     O          E            findett     2
070300000613     O                                              'Riferimento Strategi.....:'
070400000613     O                       vlrhdl              +1
070500000616     O          E            findett     2
070600000613     O                                              'Giusti...................:'
070700010201     O                       §CTROKVB      2   +  1
070800000616     O          E            findett     2
070900000613     O                                              'Sbagliati e corretti.....:'
071000000613     O                       §CTRMO        2   +  1
071100000616     O          E            findett     2
071200000613     O                                              'Sbagliati e scartati.....:'
071300000613     O                       §CTRNO        2   +  1
071400000512** CMD - COMANDI CL
071500071030OVRPRTF FILE(TITV1C1P)  TOFILE(TIS7T7P)  OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
071600071030OVRPRTF FILE(TITV1C1PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
071700071030DLTOVR FILE(TITV1C1P TITV1C1PS) LVL(*)
071800000512
071900000512
