000100120611      /TITLE Upload via Internet: traduzione in FIVAB/FIVAT x GRUPPO IRCA (ZOPPAS)
000200040706     H*                           Cod. 0590376 / 0590499 / 0590422 / 0590502 / 0590098 / 0590132
000300060523     H*                                0592665 / 0592966 / 0592967 / ...
000400990908     H dftactgrp(*yes)
000500000313     F*
000600990910     Ftivin00r  uF   E             DISK    usropn
000700021014     FFIVABwwr  O    E             DISK    usropn
000800120611     FFIVATwwr  O    E             DISK    usropn
000900000313     D*
001000010330     D*----------------------------------------------------
001100010330     D* DICHIARAZIOINE VARIABILI DI WRK
001200010330     D*----------------------------------------------------
001300010330     D dscmz         e ds                  inz
001400010330     D psds           sds
001500010330     D  procname         *PROC
001600010330     D tivlrds       e ds                  extname(tivlr00f)
001700010330     D esito           s              1
001800010330     D prmlit          s             10
001900010330     D prmfir          s             10
002000010330     D wrkesito        s                   like(esito)
002100010330     D rrnum           s              6  0 INZ(*zeros)
002200120611     D parccm          s              8    INZ(*blanks)
002300120611     D parmbr          s             10    INZ(*blanks)
002400120611     D paropz          s              1    INZ(*blanks)
002500120611     D chkcall         s              1    INZ(*blanks)
002600010330
002700010330
002800010330
002900000913     C                   reset                   rrnum
003000990921     C                   reset                   esito
003100990921     C                   reset                   wrkesito
003200010601     C*
003300010601     C                   exsr      opeini
003400010605     C                   exsr      rwvab
003500120611     C*
003600120611     C* Effettuo la chiamata al CLLE preposto
003700120611     C                   call(e)   'TITVVTC'
003800120611     C                   parm                    parccm
003900120611     C                   parm                    parmbr
004000120611     C                   parm      '2'           paropz
004100010601     C*
004200010601     C                   seton                                        lr
004300010601
004400010601
004500010601
004600010601
004700010601     C*--------------------------------------------------------
004800010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
004900010601     C*--------------------------------------------------------
005000010601     C     PREELA        BEGSR
005100010601     C*
005200010601     C* SE OCCORRE SPEDIRE IN FILIALE
005300010601     C                   if        invfil <> *zeros and
005400010601     C                             flgGiro = '0'
005500010601     C*
005600010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
005700010601     C                   eval      flgGiro = '1'
005800010601     C*
005900010601     C                   endif
006000010601     C*
006100010601     C                   ENDSR
006200010601     C***
006300010601
006400010601
006500010601
006600010601     C*--------------------------------------------------------
006700010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
006800010601     C*--------------------------------------------------------
006900010601     C     ENDELA        BEGSR
007000120611     C*
007100000616     C*
007200010601     C                   ENDSR
007300010601     C***
007400000613
007500010601
007600010601
007700010330     C*--------------------------------------------------------
007800040301     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
007900010330     C*--------------------------------------------------------
008000010605     C     RWVAB         BEGSR
008100010330     C*
008200010330     C                   if        not %open(tivin00r)
008300010330     C                   open      tivin00r
008400010330     C                   endif
008500021014     C                   if        not %open(fivabwwr)
008600021014     C                   open      fivabwwr
008700010330     C                   endif
008800120611     C*
008900120611     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
009000120611     C                   exsr      prevat
009100120611     C*
009200120611     C                   if        chkcall = '0'
009300120611     C*
009400120611     C                   if        not %open(fivatwwr)
009500120611     C                   open      fivatwwr
009600120611     C                   endif
009700010330     C*
009800120611     C                   clear                   §CTROKVB          7 0
009900120611     C                   clear                   §CTROKVT          7 0
010000010604     C                   clear                   §CTRMO
010100010604     C                   clear                   §CTRNO
010200120615     C*
010300120615     C                   exsr      inzvar
010400120615     C                   exsr      defcam
010500010330     C*
010600010330     C                   DO        *HIVAL
010700010330     C*
010800010330     C                   READ      tivin00r                               70
010900010618     C*
011000010618     C* Dopo ogni lettura verifico se ci sono stati record OK
011100010618     C                   if        vinflg = '1'
011200010618     C                   eval      flgOk = '1'
011300010618     C                   endif
011400010618     C*
011500010330     C                   if        vindta > *blanks
011600010330     C                   add       1             rrnum
011700010330     C*
011800010601     C                   if        *in70 = *off and
011900010330     C                             (vinflg = *blanks
012000010330     C                              or vinflg = '0'
012100010330     C                              or vinflg = '2')
012200010330     C*
012300010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
012400010711     C                   if        vinflg = *blanks or vinflg = '0'
012500010711     C                   clear                   vinmsg
012600010711     C                   endif
012700010601     C*
012800120611     C                   if        %subst(vindta:33:2)  = '20' and
012900120611     C                             %subst(vindta:106:1) = 'S'
013000120611     C*
013100120611     C* Tipo record dettaglio
013200120611     C                   exsr      impvat
013300120611     C*
013400120611     C* Scarico il buffer del FIVAT
013500120611     C  N31              WRITE     FIVAT000
013600120611     C  N31              add       1             §CTROKVT
013700120611     C*
013800120611     C                   else
013900120611     C*
014000120611     C* Tipo record testata
014100010605     C                   exsr      impvab
014200010601     C*
014300010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
014400010605     C                   eval      depfil = VABLNP
014500010601     C                   exsr      repfil
014600010601     C                   if        depfil = invfil
014700021025     C                   if        vlrpoi = 999
014800021025     C                   MOVE(P)   invfil        VABFGS
014900021025     C                   else
015000021025     C                   MOVE(P)   vlrpoi        VABFGS
015100021025     C                   endif
015200010601     C*
015300010601     C                   exsr      PREELA
015400010601     C*
015500010604     C* Ebbene...
015600010604     C*
015700010604     C   32              ADD       1             §CTRMO            7 0
015800010604     C   31              ADD       1             §CTRNO            7 0
015900020205     C*
016000040301     C* Scarico il buffer del FIVAB
016100021014     C  N31              WRITE     FIVAB000
016200120615     C  N31              add       1             §CTROKVB
016300120611     C                   endif
016400010604     C*
016500020722     C                   endif
016600020722     C*
016700010604     C                   if        *in31 = *off and
016800010604     C                             *in32 = *off
016900010604     C                   eval      vinflg = '1'
017000010604     C                   else
017100010604     C                   eval      vinflg = '2'
017200010604     C                   endif
017300010604     C                   endif
017400010604     C*
017500010330     C                   else
017600010330     C                   eval      vinflg = '1'
017700010330     C                   endif
017800010601     C*
017900010601     C  N70              update    tivin000
018000010330     C*
018100010330     C  N70              ENDdo
018200120611     C*
018300120611     C                   endif                                                  (endif - chkcall)
018400010601     C*
018500010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
018600010601     C                   if        cntNonEl = *zeros or
018700010601     C                             flgMulti = '0'
018800010330     C* Se non ci sono record con errori ...
018900010601     C                   if        §ctrno = 0 and
019000010604     C                             §ctrmo = 0 and
019100010601     C                             flgStato <> '2'
019200010330     C* ... restituisco esito OK.
019300010330     C                   eval      wrkesito = '0'
019400010330     C                   else
019500120611     C                   if        §ctrokvb > 0
019600010330     C                   eval      wrkesito = '1'
019700010330     C                   else
019800010615     C                   if        flgOk = '0'
019900010615     C                   eval      wrkesito = '2'
020000010615     C                   else
020100010615     C                   eval      wrkesito = '6'
020200010615     C                   endif
020300010330     C                   endif
020400010330     C                   endif
020500010601     C                   else
020600010601     C                   eval      wrkesito = '9'
020700010601     C                   endif
020800010330     C*
020900010330     C                   if        %open(tivin00r)
021000010330     C                   close     tivin00r
021100010330     C                   endif
021200021014     C                   if        %open(fivabwwr)
021300021014     C                   close     fivabwwr
021400010330     C                   endif
021500120611     C                   if        %open(fivatwwr)
021600120611     C                   close     fivatwwr
021700120611     C                   endif
021800010601     C*
021900010601     C                   if        vlrpoi <> 999
022000010601     C                   eval      invfil = vlrpoi
022100010601     C                   endif
022200010330     C*
022300120611     C                   if        §ctrokvb > 0
022400010601     C                             and invfil > *zeros
022500010330     C                   exsr      invio
022600010330     C                   endif
022700010601     C*
022800010618     C                   if        flgGiro = '1'
022900010601     C                   exsr      endela
023000010618     C                   endif
023100010330     C*
023200010330     C                   ENDSR
023300010330     C***
023400120611
023500120611
023600120611
023700120611     C*----------------------------------------------------*
023800120611     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
023900120611     C*----------------------------------------------------*
024000120611     C     PREVAT        BEGSR
024100120611     C*
024200120611     C* Compongo il nome del membro da dare al FIVATWWR
024300120611     C                   eval      parmbr = vlrhdl
024400120611     C                   movel     'M'           parmbr
024500120611     C                   eval      parccm = vlrksc
024600120611     C                   eval      paropz = '1'
024700120611     C* Effettuo la chiamata al CLLE preposto
024800120611     C                   call(e)   'TITVVTC'
024900120611     C                   parm                    parccm
025000120611     C                   parm                    parmbr
025100120611     C                   parm                    paropz
025200120611     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
025300120611     C                   if        %error
025400120611     C                   movel     '1'           chkcall
025500120611     C                   else
025600120611     C                   movel     '0'           chkcall
025700120611     C                   endif
025800120611     C*
025900120611     C                   ENDSR
026000010601
026100010601
026200010601
026300010330     C*----------------------------------------------------*
026400020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
026500010330     C*----------------------------------------------------*
026600010330     C     INZVAR        BEGSR
026700010330     C*
026800020204     C                   Z-ADD     *zeros        Num5_0            5 0
026900020322     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
027000020322     C                   MOVEL     '0'           FlgCAS            1
027100010330     C*
027200010330     C                   ENDSR
027300010330     C*----------------------------------------------------*
027400020722     C*  IMPOSTAZIONE CAMPI COSTANTI
027500010330     C*----------------------------------------------------*
027600010330     C     DEFCAM        BEGSR
027700010330     C*
027800020204     C* Inizializzo il buffer del record da scrivere
027900021014     C                   CLEAR                   FIVAB000
028000120611     C                   CLEAR                   FIVAT000
028100020204     C* Imposto i valori di default...
028200040706     C                   EVAL      VABLNP = 059
028300040706     C                   EVAL      VABCTR = 000
028400040706     C                   EVAL      VABTSP = 'C'
028500040706     C                   EVAL      VABCBO = '1'
028600120611     C                   EVAL      VABCTM = '7Q'
028700040707     C                   MOVEL     'I'           wIOE              1
028800060523     C                   MOVEL     'RICA'        wDIV              4
028900060523     C                   MOVEL     'F'           wPROD             1
029000020204     C*
029100010330     C                   ENDSR
029200010607     C*----------------------------------------------------*
029300021014     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
029400010607     C*----------------------------------------------------*
029500010607     C     IMPVAB        BEGSR
029600010607     C*
029700010607     C                   SETOFF                                       3132
029800010607     C*
029900010607     C* Reperimento campi ALFA
030000010607     C*
030100010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
030200040706     C                   EVAL      VABRSD=%trim(%subst(vindta:225:35))
030300020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
030400020117     C     '@':'A'       XLATE     VABRSD        VABRSD
030500020117     C* ==
030600040706     C                   EVAL      VABIND=%trim(%subst(vindta:260:35))
030700040706     C                   EVAL      VABLOD=%trim(%subst(vindta:300:25))
030800040706     C                   EVAL      VABPRD=%trim(%subst(vindta:325:2))
030900040707     C***                EVAL      VABNZD=%trim(%subst(vindta:327:4))
031000040708     C                   EVAL      VABRMA=%trim(%subst(vindta:7:16))
031100040706     C                   EVAL      VABNOT=%trim(%subst(vindta:388:35))
031200130212     C                   EVAL      VABNT2=%trim(%subst(vindta:388+35:33))
031300040706     C                   IF        %trim(%subst(vindta:347:10)) = 'VE019'
031400040706     C                   EVAL      VABTSP='C'
031500040706     C                   ENDIF
031600040706     C                   IF        %trim(%subst(vindta:347:10)) = 'VE094'
031700040706     C                   EVAL      VABTSP='E'
031800040706     C                   ENDIF
031900141021     C                   IF        %trim(%subst(vindta:347:10)) = '19'
032000141021     C                   EVAL      VABTSP='C'
032100141021     C                   ENDIF
032200141021     C                   IF        %trim(%subst(vindta:347:10)) = '94'
032300141021     C                   EVAL      VABTSP='E'
032400141021     C                   ENDIF
032500040706     C                   IF        %trim(%subst(vindta:382:4)) = 'DDU'
032600040706     C                   EVAL      VABCBO='1'
032700040706     C                   ENDIF
032800040706     C                   IF        %trim(%subst(vindta:382:4)) = 'EXW'
032900040706     C                   EVAL      VABCBO='2'
033000040706     C                   ENDIF
033100040706     C                   IF        %trim(%subst(vindta:386:2)) = 'SI'
033200040706     C                   EVAL      FlgCAS='1'
033300040706     C                   ENDIF
033400040707     C*
033500040707     C* Considerazioni particolari su spedizione ITALIA o ESTERO
033600060523     C                   IF        %subst(vindta:23:3) = 'DDT'
033700060523     C                   MOVEL     'I'           wIOE
033800060523     C                   ELSE
033900040707     C                   MOVEL     'E'           wIOE
034000040707     C                   ENDIF
034100060523     C*
034200060523     C* Considerazioni particolari su prodotto FINITO o c/to LAVORO
034300060523     C                   IF        wIOE = 'I'
034400060523     C                   IF        %subst(vindta:28:1) = '0' OR
034500060523     C                             %subst(vindta:28:1) = '2' OR
034600060523     C                             %subst(vindta:28:1) = '3'
034700060523     C                   MOVEL     'F'           wPROD
034800060523     C                   ELSE
034900060523     C                   IF        %subst(vindta:28:1) = '6' OR
035000060523     C                             %subst(vindta:28:1) = '7' OR
035100060523     C                             %subst(vindta:28:1) = '8'
035200060523     C                   MOVEL     'L'           wPROD
035300060523     C                   ENDIF
035400060523     C                   ENDIF
035500060523     C                   ENDIF
035600060523     C*
035700060523     C* Considerazioni particolari sulla DIVISIONE
035800171010     C* le divisioni numeriche non dovrebbero più essere usate
035900171010     C* (le tengo per completezza)
036000171010     C                   IF        %subst(vindta:1:6) = '000021' or
036100171010     C                             %trim(%subst(vindta:1:6)) = 'IRI1'
036200060523     C*    */ Divisione RICA */
036300060531     C                   MOVEL(P)  'RICA'        wDIV
036400060523     C                   ELSE
036500171010     C                   IF        %subst(vindta:1:6) = '000022' or
036600171010     C                             %trim(%subst(vindta:1:6)) = 'IEI1'
036700060523     C*    */ Divisione SEV */
036800060531     C                   MOVEL(P)  'SEV'         wDIV
036900060523     C                   ELSE
037000171010     C                   IF        %subst(vindta:1:6) = '000023' or
037100171010     C                             %trim(%subst(vindta:1:6)) = 'III1'
037200060523     C*    */ Divisione IRCA */
037300060531     C                   MOVEL(P)  'IRCA'        wDIV
037400060523     C                   ENDIF
037500060523     C                   ENDIF
037600060523     C                   ENDIF
037700060523     C*
037800060523     C* Considerazioni finali sull'attribuzione del codice cliente mittente
037900060523     C                   IF        wDIV = 'IRCA'
038000171010     C* tutti i test in più rispetto alla divisione non dovrebbero essere più attivi
038100171010     C* (li tengo per completezza)
038200060523     C                   IF        wIOE = 'I' AND wPROD = 'F'
038300060523     C                   EVAL      VABCCM = 0590098
038400060523     C                   ENDIF
038500060523     C                   IF        wIOE = 'I' AND wPROD = 'L'
038600060523     C                   EVAL      VABCCM = 0592965
038700060523     C                   ENDIF
038800060523     C                   IF        wIOE = 'E'
038900060523     C                   EVAL      VABCCM = 0590132
039000060523     C                   ENDIF
039100171010     C* per la divisione IRCA il cod.cliente è questo
039200171010     C                   EVAL      VABCCM = 0590098
039300060523     C                   ENDIF
039400060523     C*
039500060523     C                   IF        wDIV = 'RICA'
039600171010     C* tutti i test in più rispetto alla divisione non dovrebbero essere più attivi
039700171010     C* (li tengo per completezza)
039800060523     C                   IF        wIOE = 'I' AND wPROD = 'F'
039900060523     C                   EVAL      VABCCM = 0590376
040000060523     C                   ENDIF
040100060523     C                   IF        wIOE = 'I' AND wPROD = 'L'
040200060523     C                   EVAL      VABCCM = 0592966
040300060523     C                   ENDIF
040400060523     C                   IF        wIOE = 'E'
040500060523     C                   EVAL      VABCCM = 0590499
040600060523     C                   ENDIF
040700171010     C* per la divisione RICA il cod.cliente è questo
040800171010     C                   EVAL      VABCCM = 0590376
040900060523     C                   ENDIF
041000060523     C*
041100060523     C                   IF        wDIV = 'SEV'
041200171010     C* tutti i test in più rispetto alla divisione non dovrebbero essere più attivi
041300171010     C* (li tengo per completezza)
041400060523     C                   IF        wIOE = 'I' AND wPROD = 'F'
041500060523     C                   EVAL      VABCCM = 0590422
041600060523     C                   ENDIF
041700060523     C                   IF        wIOE = 'I' AND wPROD = 'L'
041800060523     C                   EVAL      VABCCM = 0592967
041900060523     C                   ENDIF
042000060523     C                   IF        wIOE = 'E'
042100060523     C                   EVAL      VABCCM = 0590502
042200060523     C                   ENDIF
042300171010     C* per la divisione IRCA ENG (SEV) il cod.cliente è questo
042400171010     C                   EVAL      VABCCM = 0590422
042500060523     C                   ENDIF
042600040707     C*
042700040707     C* Considerazioni particolari sulla tariffa ITALIA o ESTERO
042800040707     C                   IF        wIOE = 'E'
042900040707     C                   EVAL      VABCTR = 099
043000040707     C                   ENDIF
043100060523     C*
043200040706     C                   IF        VABCCM = *zeros
043300040706     C                   SETON                                        31
043400040706     C                   ENDIF
043500010607     C*
043600040706     C* Reperimento campi NUMERICI
043700020722     C                   MOVEL     DATCOR        VABAAS
043800020722     C                   MOVE      DATCOR        VABMGS
043900040420     C* NSP/RMN
044000040707     C                   Z-ADD     *zeros        wPOS              2 0
044100040707     C                   EVAL      wPOS=%scan('/':%subst(vindta:23:10))
044200040707     C                   EVAL      PiStr=%trim(%subst(vindta:23+wPOS:10-wPOS))
044300020304     C                   EXSR      CHKNUM
044400020304     C                   IF        PiInt=*on
044500040301     C                   Z-ADD     PiVal         VABNSP
044600040301     C                   Z-ADD     PiVal         VABRMN
044700020304     C                   ELSE
044800020722     C                   SETON                                        31
044900020304     C                   Z-ADD     *zeros        VABNSP
045000040301     C                   Z-ADD     *zeros        VABRMN
045100020304     C                   EVAL      vinmsg = %trimr(vinmsg)
045200040301     C                             + ' ' + 'VABNSP VABRMN'
045300020304     C                   ENDIF
045400040420     C* CAD
045500040706     C                   EVAL      PiStr=%trim(%subst(vindta:295:5))
045600010607     C                   EXSR      CHKNUM
045700010607     C                   IF        PiInt=*on
045800020204     C                   Z-ADD     PiVal         Num5_0
045900020204     C                   MOVEL(p)  Num5_0        VABCAD
046000010607     C                   ELSE
046100010607     C                   SETON                                        32
046200020204     C                   EVAL      VABCAD = *zeros
046300010607     C                   EVAL      vinmsg = %trimr(vinmsg)
046400020204     C                             + ' ' + 'VABCAD'
046500010607     C                   ENDIF
046600040420     C* NCL
046700040706     C                   EVAL      PiStr=%trim(%subst(vindta:357:5))
046800010607     C                   EXSR      CHKNUM
046900010607     C                   IF        PiInt=*on
047000010607     C                   Z-ADD     PiVal         VABNCL
047100010607     C                   ELSE
047200010607     C                   SETON                                        32
047300010607     C                   Z-ADD     *zeros        VABNCL
047400010607     C                   EVAL      vinmsg = %trimr(vinmsg)
047500010607     C                             + ' ' + 'VABNCL'
047600010607     C                   ENDIF
047700040420     C* PKB
047800040706     C                   EVAL      PiStr=%trim(%subst(vindta:362:10))
047900010607     C                   EXSR      CHKNUM
048000010607     C                   IF        PiNum=*on
048100040420     C                   Z-ADD     PiVal         VABPKB
048200040608     C                   EVAL(H)   VABPKB = PiVal/1000                          * gestisco 3 dec.
048300010607     C                   ELSE
048400010607     C                   SETON                                        32
048500010607     C                   Z-ADD     *zeros        VABPKB
048600010607     C                   EVAL      vinmsg = %trimr(vinmsg)
048700010607     C                             + ' ' + 'VABPKB'
048800010607     C                   ENDIF
048900040706     C* VLB
049000040706     C                   EVAL      PiStr=%trim(%subst(vindta:372:10))
049100040706     C                   EXSR      CHKNUM
049200040706     C                   IF        PiNum=*on
049300040706     C                   Z-ADD     PiVal         VABVLB
049400040706     C                   EVAL(H)   VABVLB = PiVal/1000                          * gestisco 3 dec.
049500040706     C                   ELSE
049600040706     C                   SETON                                        32
049700040706     C                   Z-ADD     *zeros        VABVLB
049800040706     C                   EVAL      vinmsg = %trimr(vinmsg)
049900040706     C                             + ' ' + 'VABVLB'
050000040706     C                   ENDIF
050100130212     C* CAS
050200130212     C                   IF        %subst(vindta:588:10) <> *blanks
050300130212     C                   EVAL      VABTIC = 'BM'
050400130228     C                   EVAL      vabVCA = 'EUR'
050500130212     C                   EVAL      PiStr=%trim(%subst(vindta:588:10))
050600130212     C                   EXSR      CHKNUM
050700130212     C                   IF        PiNum=*on
050800130212     C                   Z-ADD     PiVal         VABCAS
050900130212     C                   EVAL(H)   VABCAS = PiVal/100                           * gestisco 2 dec.
051000130212     C                   ELSE
051100130212     C                   SETON                                        32
051200130212     C                   Z-ADD     *zeros        VABCAS
051300130212     C                   EVAL      vinmsg = %trimr(vinmsg)
051400130212     C                             + ' ' + 'VABCAS'
051500130212     C                   ENDIF
051600130212     C                   ENDIF
051700020322     C*
051800020322     C* Considerazioni finali su CBO/CAS
051900020322     C                   IF        FlgCAS = '1'
052000040706     C                   IF        VABCBO = '1'
052100020322     C                   EVAL      VABCBO = '4'
052200040706     C                   ENDIF
052300040706     C                   IF        VABCBO = '2'
052400040706     C                   EVAL      VABCBO = '6'
052500020322     C                   ENDIF
052600040706     C                   ENDIF
052700020204     C*
052800020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
052900020204     C                   EXSR      CHKIMPDIV
053000020204     C*
053100010607     C                   ENDSR
053200010607     C*----------------------------------------------------*
053300120611
053400120611
053500120611     C*----------------------------------------------------*
053600120611     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
053700120611     C*----------------------------------------------------*
053800120611     C     IMPVAT        BEGSR
053900120611     C*
054000120611     C                   SETOFF                                       31
054100120611     C*
054200120627     C                   EVAL      VATFGS = VABFGS
054300120611     C                   EVAL      VATAAS = VABAAS
054400120611     C                   EVAL      VATLNP = VABLNP
054500120611     C                   EVAL      VATNRS = VABNRS
054600120611     C                   EVAL      VATNSP = VABNSP
054700120611     C                   EVAL      VATCCM = VABCCM
054800120611     C*
054900120611     C                   EVAL      VATTRC = 'E'
055000120611     C                   EVAL      VATNOT = %trim(%subst(vindta:45:11))
055100120611     C*
055200120627     C                   IF        VATAAS = *zeros  AND
055300120627     C                             VATLNP = *zeros  AND
055400120627     C                             VATNRS = *zeros  AND
055500120627     C                             VATNSP = *zeros  AND
055600120627     C                             VATCCM = *zeros  AND
055700120611     C                             VATNOT = *blanks
055800120611     C                   SETON                                        31
055900120611     C                   ENDIF
056000120611     C*
056100120611     C                   ENDSR
056200120611     C*----------------------------------------------------*
056300010601
056400020204
056500020204     C*----------------------------------------------------*
056600020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
056700020204     C*----------------------------------------------------*
056800020204     C     CHKIMPDIV     BEGSR
056900020204     C*
057000020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
057100020204     C                   Z-ADD     *zeros        wrkDec            9 9
057200020204     C*
057300020204     C* Come prima cosa effettuo considerazioni sulla divisa
057400020204     C                   IF        vabIAS > *zeros
057500020204     C                   IF        vabVAS <> 'EUR'
057600020204     C                   EVAL      vabVAS =  'ITL'
057700020204     C                   ENDIF
057800020204     C                   ENDIF
057900020204     C*
058000020204     C                   IF        vabCAS > *zeros
058100020204     C                   IF        vabVCA <> 'EUR'
058200020204     C                   EVAL      vabVCA =  'ITL'
058300020204     C                   ENDIF
058400020204     C                   ENDIF
058500020204     C*
058600020204     C                   IF        vabVMD > *zeros
058700020321     C                   IF        vabVAD <> 'EUR'
058800020204     C                   EVAL      vabVAD =  'ITL'
058900020204     C                   ENDIF
059000020204     C                   ENDIF
059100020204     C*
059200020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
059300020204     C                   Z-ADD     vabIAS        wrkDec
059400020204     C                   IF        wrkDec > *zeros
059500020204     C                   IF        vabVAS = 'ITL'
059600020204     C                   EVAL      vabIAS = *zeros
059700020204     C                   ENDIF
059800020204     C                   ENDIF
059900020204     C*
060000020204     C* Stabilisco se il contrasegno ha decimali valorizzati
060100020204     C                   Z-ADD     vabCAS        wrkDec
060200020204     C                   IF        wrkDec > *zeros
060300020204     C                   IF        vabVCA = 'ITL'
060400020204     C                   EVAL      vabCAS = *zeros
060500020204     C                   ENDIF
060600020204     C                   ENDIF
060700020204     C*
060800020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
060900020204     C                   Z-ADD     vabVMD        wrkDec
061000020204     C                   IF        wrkDec > *zeros
061100020204     C                   IF        vabVAD = 'ITL'
061200020204     C                   EVAL      vabVMD = *zeros
061300020204     C                   ENDIF
061400020204     C                   ENDIF
061500020204     C*
061600020204     C                   ENDSR
061700020204     C***
061800020204
061900010330
062000010330
062100010330     C*----------------------------------------------------*
062200010330     C*  CONTROLLO NUMERICITA' CAMPI
062300010330     C*----------------------------------------------------*
062400010330     C     CHKNUM        BEGSR
062500010330     C*
062600010606     C                   IF        PiDecChr = *blanks
062700010606     C                   EVAL      PiDecChr = ','
062800010606     C                   ENDIF
062900010606     C*
063000010606     C                   CALL(e)   'ISNUMERIC'
063100010330     C                   PARM                    PiStr            30
063200010606     C                   PARM                    PiDecChr          1
063300010330     C                   PARM      *ZEROS        PiVal            30 9
063400010330     C                   PARM      '0'           PiInt             1
063500010330     C                   PARM      '0'           PiNum             1
063600010330     C                   IF        %error
063700010606     C                   EVAL      PiNum=*off
063800010330     C                   ENDIF
063900010330     C*
064000010330     C                   ENDSR
064100010330     C***
064200010330
064300010601
064400010601
064500010601
064600010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
064700010601     C     repfil        BEGSR
064800010601     C*
064900010601     C                   if        invfil = *zeros and
065000010601     C                             depfil > *zeros and
065100010629     C                             (vinflg = *blanks or
065200010629     C                              vinflg = *zeros)
065300010601     C
065400010601     C                   eval      invfil = depfil
065500010601     C                   endif
065600010601     C*
065700010601     C                   if        depfil <> invfil and
065800010601     C                             invfil > *zeros
065900010601     C                   eval      flgMulti = '1'
066000010601     C                   if        vinflg = *blanks
066100010601     C                   add       1             cntNonEl
066200010601     C                   endif
066300010601     C                   endif
066400010601     C*
066500010601     C                   if        vinflg = '2'
066600010601     C                   eval      flgStato = '2'
066700010601     C                   endif
066800010601     C*
066900010601     C                   ENDSR
067000010601     C***
067100120611
067200120611
067300120611
067400120611      /TITLE Invio dei dati al punto operativo.
067500120611     C     invio         BEGSR
067600120611     C*
067700120611     C* 1° invio FIVAT
067800120611     C                   reset                   dscmz
067900120611     C                   move      vlrpoi        cmzdst
068000120611     C                   eval      cmzfld = 'FIVATWWR'
068100120611     C                   eval      cmzmbd = vlrhdl
068200120611     C                   eval      %subst(cmzmbd:1:1) = 'M'
068300120611     C***                if        prmfir = *blanks
068400120611     C                   eval      cmzfla = 'FIVAT00F'
068500120611     C                   eval      cmzmba = 'FIVAT00F'
068600120611     C***                else
068700120611     C***                eval      cmzfla = prmfir
068800120611     C***                eval      cmzmba = prmfir
068900120611     C***                endif
069000120611     C                   eval      cmznrr = *zeros
069100120611     C                   move      §ctrokvt      cmznrr
069200120611     C                   eval      cmzlba = vlrfl1
069300120611     C                   call(e)   'TIS711C'
069400120611     C                   parm                    dscmz
069500120611     C                   parm      *blanks       esito
069600120611     C                   if        %error
069700120611     C                             or cmzerr = '1'
069800120611     C                             or esito  = '1'
069900120611     C                   eval      wrkesito = '3'
070000120611     C                   else
070100120611     C*
070200120611     C* 2° invio FIVAB
070300120611     C                   reset                   dscmz
070400120611     C                   move      vlrpoi        cmzdst
070500120611     C                   eval      cmzfld = vlrfou
070600120611     C                   eval      cmzmbd = vlrhdl
070700120611     C                   eval      %subst(cmzmbd:1:1) = 'M'
070800120611     C***                if        prmfir = *blanks
070900120611     C                   eval      cmzfla = 'FIVAB00F'
071000120611     C                   eval      cmzmba = 'FIVAB00F'
071100120611     C***                else
071200120611     C***                eval      cmzfla = prmfir
071300120611     C***                eval      cmzmba = prmfir
071400120611     C***                endif
071500120611     C                   eval      cmznrr = *zeros
071600120611     C                   move      §ctrokvb      cmznrr
071700120611     C                   eval      cmzlba = vlrfl1
071800120611     C                   call(e)   'TIS711C'
071900120611     C                   parm                    dscmz
072000120611     C                   parm      *blanks       esito
072100120611     C                   if        %error
072200120611     C                             or cmzerr = '1'
072300120611     C                             or esito  = '1'
072400120611     C                   eval      wrkesito = '3'
072500120611     C                   endif
072600120611     C                   endif
072700120611     C*
072800120611     C                   ENDSR
072900120611     C***
073000010601
073100010601
073200010601
073300010601      /TITLE Invio dei dati al punto operativo.
073400010601     C     opeini        BEGSR
073500010601     C*
073600010601     C* Inizializzo flag e contatori operativi
073700010601     C                   movel     '0'           flgGiro           1
073800010601     C                   movel     '0'           flgMulti          1
073900010601     C                   movel     '1'           flgStato          1
074000010615     C                   movel     '0'           flgOk             1
074100010601     C                   z-add     *zeros        cntNonEl         10 0
074200010601     C                   z-add     *zeros        depfil            3 0
074300010601     C                   z-add     *zeros        invfil            3 0
074400010601     C*
074500010601     C                   ENDSR
074600010601     C***
074700120611
074800120611
074900120611
075000120611     C     *pssr         BEGSR
075100120611     C*
075200120611     C                   if        %open(tivin00r)
075300120611     C                   close     tivin00r
075400120611     C                   endif
075500120611     C                   if        %open(fivabwwr)
075600120611     C                   close     fivabwwr
075700120611     C                   endif
075800120611     C                   if        %open(fivatwwr)
075900120611     C                   close     fivatwwr
076000120611     C                   endif
076100120611     C*
076200120611     C* Effettuo la chiamata al CLLE preposto
076300120611     C                   call(e)   'TITVVTC'
076400120611     C                   parm                    parccm
076500120611     C                   parm                    parmbr
076600120611     C                   parm      '2'           paropz
076700120611     C*
076800120611     C                   eval      wrkesito = '2'
076900120611     C*
077000120611     C                   seton                                        LR
077100120611     C*
077200120611     C                   ENDSR     '*CANCL'
077300120611     C***
077400010601
077500010330
077600010330
077700000613     C     *inzsr        BEGSR
077800990910     C*
077900990910     C     *entry        plist
078000990920     C                   parm                    tivlrds
078100990921     C                   parm      wrkesito      esito
078200000724     C                   parm                    prmlit
078300000710     C                   parm                    prmfir
078400010330     C*
078500010330     C* CALCOLA LA DATA CORRENTE
078600120611     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
078700120611     C                   eval      datcor = %dec(%date() : *ISO)
078800000613     C*
078900000613     C                   ENDSR
079000000613     C***
