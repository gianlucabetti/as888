000100120319     H DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio) BNDDIR('TIS')
000200160502     h dftactgrp(*no)
000300970703     H* FNLV13R  *---------------------------------------------------*
000400970703     H*     CONTROLLO CAP
000500921021     H*--------------------------------------------------------------*
000600970708     H* CONPORTAMENTI DEL TISI95R
000700970708     H* 1) SE TIPO CONTROLLO 7:
000800970708     H*   *CHIEDE GIA' DA SOLO LA PROVINCIA
000900970708     H*                         OBBLIGATORIA PER ITALIA (NON PER 3 E 4)
001000970708     H*   *PUO' RESTITUIRE I SEGUENTI DATI MODIFICATI IN AUTOMATICO :
001100970708     H*         CAP  NAZIONE  E  PROVINCIA
001200970708     H*         EMETTERE MSG DI AVVISO EVIDENZIANDO I DATI
001300970708     H* 2) PER ANDARE IN INTERROGAIZONE CAPPARIO IMMETTERE LA LOCALITA'
001400970708     H*    SENZA LA PROVINCIA E IL CAP
001500970708     H*--------------------------------------------------------------*
001600080414     D ERR             S             78    DIM(19) CTDATA PERRCD(1)
001700970701     D* DS PER TISI95R - CONTROLLO DI UN CAP
001800970701     D DSSI95        E DS                  EXTNAME(TISI95DS)
001900970707     D DSSI00        E DS                  EXTNAME(TISI00DS)
002000150731     D tisi30ds      E DS
002100970707     D DSLV13        E DS                  EXTNAME(FNLV13DS)
002200060921     D TISI97DS      E DS
002300061030     D dcppflo       E DS
002400970707     D KPJBA         E DS
002500000721     D                SDS
002600000721     D  PARMS                 37     39  0
002700120319     ***************************************************************************
002800120319     D rtvMsgLang      PR          3512A
002900120319     D  msgId                         7A   CONST
003000120322     D  pilinguaISO2                  2A   OPTIONS(*OMIT:*NOPASS)
003100120319     D  piMsgDta                    512A   OPTIONS(*OMIT:*NOPASS:*VARSIZE) CONST
003200120319     D  piMsg                       644A   OPTIONS(*OMIT:*NOPASS)
003300120319     D                                     VARYING
003400120319     D  piSecLvl                   3512A   OPTIONS(*OMIT:*NOPASS)
003500120319     D                                     VARYING
003600120319     D  piRtnCode                    10A   OPTIONS(*OMIT:*NOPASS)
003700120319     D  piEsito                      15P 0 OPTIONS(*OMIT:*NOPASS)
003800120319     d
003900120322     D  klin           s              2
004000120322     D  cont           s              3  0
004100120321     D  Leng           s              3  0
004200120321     D  messag         s            250
004300120321     D  messag_uni     s            250
004400150121     D  inx            s              3  0
004500150121     D  w_captll       s                   like(o95cap)
004600150121
004700150121      /COPY GAITRASRC/SRCUB,UBTL05D02
004800150121     D   pInNazione    S              3A
004900150121     D   pInProvincia  S              4A
005000150121     D   pInComune     S            128A
005100150121     D   pInLocalita   S            128A
005200150121     D   pInCAP        S              9A
005300150121     D   pInVia        S            128A
005400150121     D   pInCivico     S             10A
005500150121     D   pInScore      S             10A
005600150121     D   pOutEsito     S              1A
005700150121     D   pOutEsitoDes  S            128A
005800150121     D   pOutTotFound  S              4S 0
005900150121     D   pOutResults   DS                  dim(10) likeds(dsOutUBTL05R02)
006000150121
006100921021     C*---------------------------------------------------------------*
006200921021     C     *ENTRY        PLIST
006300970707     C                   PARM                    KPJBA
006400970707     C                   PARM                    DSLV13
006500970707     C                   PARM                    DSSI95
006600000721     C                   PARM                    FLGBAC            1
006700060921     C                   PARM                    tisi97ds
006800120322     C                   PARM                    lingua            2
006900940218     C*---------------------------------------------------------------*
007000970707     C* TIPO LANCIO, I13TLA "C" -           CHIUSO CON LR
007100970707     C* TIPO LANCIO, I13TLA "L" - ELABORO E CHIUDO CON LR
007200970707     C* TIPO LANCIO, I13TLA " " - ELABORO E CHIUDO IN RETRN
007300970707    1C     I13TLA        IFNE      'C'
007400970707     C                   CLEAR                   O13ERR
007500970707     C                   CLEAR                   O13MSG
007600970708     C                   CLEAR                   O13ROP
007700970708     C                   CLEAR                   O13ROC
007800970708     C                   CLEAR                   O13RON
007900970708     C                   CLEAR                   O13ROL
008000970707     C**
008100970701     C                   CALL      'TISI95R'
008200970701     C                   PARM                    DSSI95
008300941019     C*
008400970703     C* NON TROVATI ERRORI
008500970707    2C     O95ERR        IFEQ      *BLANKS
008600120322     c* Se passato 6° parametro, imposto codice lingua
008700120322    3c                   if        parms=6
008800120322     c                   eval      klin =lingua
008900120322     c                   else
009000120322     c                   clear                   klin
009100120322     c                   endif
009200120319     c
009300080407     c* Se richiesto controllo cappario utilizzi particolari
009400120322    3c                   if        parms>=5
009500080407     c                   eval      o97uti='S'
009600060921     c                   call      'TISI97R'
009700060921     c                   parm                    dssi95
009800060921     c                   parm                    tisi97ds
009900060921     c* se c'e' errore imposto ed esco
010000060921    4c                   if        o97err<>' '
010100060921     C                   MOVEL     O97MSG        O13MSG
010200060921     c                   eval      o13err='5'
010300060921   x4c                   else
010400120321     c
010500120321     c* Preparo messaggio specifico
010600120321     c                   exsr      PrepaMSG
010700060921    4c                   endif
010800060921    3c                   endif
010900970707     C**
011000970707     C* LIVELLO AFFIDABILITA'0 : SEMPRE ERRORE
011100970707    3C     I13AF0        IFEQ      'S'
011200970708   3AC     O95LIA        IFEQ      '0'
011300970707     C**
011400970703     C* SE IL CAP L'HA TROVATO DO ERRORE DI INCONGRUENZA
011500970708    4C     O95LID        IFLT      '2'
011600970708     C                   MOVEL     ERR(2)        O13MSG
011700970707     C                   MOVEL     '5'           O13ERR
011800970703     C                   ELSE
011900970708     C                   MOVEL     ERR(1)        O13MSG
012000970707     C                   MOVEL     '5'           O13ERR
012100970707    4C                   ENDIF
012200970703     C*
012300970703     C                   GOTO      ENDC01
012400970708   3AC                   ENDIF
012500970708   X3C                   ELSE
012600970708    4C     O95LID        IFEQ      '1'
012700970708     C     O95NUF        ANDNE     'S'
012800970708     C                   MOVEL     ERR(2)        O13MSG
012900970708     C                   MOVEL     '5'           O13ERR
013000970710     C                   GOTO      ENDC01
013100970708    4C                   ENDIF
013200970708    3C                   ENDIF
013300970704     C*
013400970704     C* CAP E PROVINCIA DEVONO ESSERE CONGRUENTI
013500970707    3C     I13CNG        IFEQ      'S'
013600970707     C     I95TCN        ANDEQ     '3'
013700970707     C*
013800970707    4C     O95NCP        IFEQ      'S'
013900970704     C     I95PRV        ANDNE     O95PRV
014000970704     C     I95PRV        ANDNE     O95PRA
014100970708     C                   MOVEL     ERR(3)        O13MSG
014200970707     C                   MOVEL     '5'           O13ERR
014300970707     C                   GOTO      ENDC01
014400970707    4C                   ENDIF
014500970707    3C                   ENDIF
014600970708     C**
014700970708     C* SE LIVELLO DATI 3 O 4 E AFFIDABILITA' 2 O > PROPONGO I DATI
014800970708     C*  CHE SI SONO MODIFICATI CON MSG DI AVVISO
014900970708    1C     O95LID        IFEQ      '3'
015000970708     C     O95LID        OREQ      '4'
015100970708    2C     O95LIA        IFGE      '2'
015200970708     C*
015300970708    3C     O95PRV        IFNE      I95PRV
015400970714    3C     O95PRA        ANDNE     I95PRV
015500970708     C                   MOVEL     '4'           O13ERR
015600000721     C*
015700120322     C     PARMS         IFge      4
015800000721     C     FLGBAC        ANDEQ     'S'
015900000721     C                   MOVEL     ERR(10)       O13MSG
016000000721     C                   ELSE
016100970708     C                   MOVEL     ERR(8)        O13MSG
016200000721     C                   ENDIF
016300970708     C                   MOVEL     'S'           O13ROP
016400970708    3C                   ENDIF
016500970708    3C     O95NAR        IFNE      I95NAR
016600970708     C                   MOVEL     '6'           O13ERR
016700000721     C*
016800120322     C     PARMS         IFge      4
016900000721     C     FLGBAC        ANDEQ     'S'
017000000721     C                   MOVEL     ERR(10)       O13MSG
017100000721     C                   ELSE
017200970708     C                   MOVEL     ERR(8)        O13MSG
017300000721    3C                   ENDIF
017400000721     C*
017500970708     C                   MOVEL     'S'           O13RON
017600970708    3C                   ENDIF
017700150210    3C     O95CAP        IFNE      I95CAP
017800150210     c* se richiamato con utilizzo Tellus e il cap di output ha il
017900150210     c* viario richiamo tellus per cercare il cap corretto
018000150210     c                   if        i13cnv='Y' and o95flg='S'
018100150210     c                   exsr      sr_NormTell
018200150210     c                   if        w_captll<>*blanks
018300150210     c                   movel     w_captll      o95cap
018400150210     c                   eval      o95lia='3'
018500150210     c                   eval      o95flg='N'
018600150210     c                   endif
018700150210     c                   endif
018800150210     c                   if        i95cap<>o95cap
018900970708     C                   MOVEL     '5'           O13ERR
019000000721     C*
019100120322     C     PARMS         IFge      4
019200000721     C     FLGBAC        ANDEQ     'S'
019300000721     C                   MOVEL     ERR(10)       O13MSG
019400000721     C                   ELSE
019500970708     C                   MOVEL     ERR(8)        O13MSG
019600150210     C                   ENDIF
019700000721     C*
019800970708     C                   MOVEL     'S'           O13ROC
019900150210     c                   endif
020000970708    3C                   ENDIF
020100150210     C     O13rop        IFNE      ' '
020200150210     C     O13roc        orNE      ' '
020300150210     C     O13ron        orNE      ' '
020400150210     C                   GOTO      ENDC01
020500150210    2C                   ENDIF
020600970708    2C                   ENDIF
020700970708    1C                   ENDIF
020800970710     C*
020900000713     C* VEDO SE DARE ERRORE PER LIVELLO DI AFFIDABILITA' = 2 O 1
021000000713     C*  SE NON FACCIO I CONTROLLI SPECIFICI PER LIVELLO 1
021100970710     C     O95LIA        IFEQ      '2'
021200000713     C     I13AF1        OREQ      ' '
021300000713     C     O95LIA        ANDEQ     '1'
021400000713     C*
021500000713     C     O95NUF        IFNE      'S'
021600970710     C     I13LA3        ANDEQ     'S'
021700970710     C     E13FZ3        ANDNE     1
021800970710     C                   MOVEL     '5'           O13ERR
021900970710     C     I13SZ3        IFEQ      'S'
022000970710     C                   MOVEL     ERR(9)        O13MSG
022100970710     C                   MOVEL     '1'           E13FZ3
022200970710     C                   ELSE
022300970710     C                   MOVEL     ERR(1)        O13MSG
022400970710     C                   ENDIF
022500970710     C                   GOTO      ENDC01
022600970710     C                   ENDIF
022700000713     C                   ENDIF
022800970707     C*
022900970707     C* CAP CON VIARIO
023000970707    4C     O95FLG        IFEQ      'S'
023100150121     c* se richiesto controllo cap con viario + normalizzazione
023200150121     c* indirizzo tramite Tellus richiamo quest'ultimo driver
023300150123     c* non richiamo il driver se presente errore di
023400150123     c* "CAP incongruente cone provincia e/o località"
023500150123     c                   if        i13cnv='Y' and o95lia >'2'
023600150121     c                   exsr      sr_NormTell
023700150121     c                   if        w_captll<>*blanks
023800150121     c                   movel     w_captll      o95cap
023900150123     C     PARMS         IFge      4
024000150123     C     FLGBAC        ANDEQ     'S'
024100150123     C                   MOVEL     ERR(10)       O13MSG
024200150123     C                   ELSE
024300150123     C                   MOVEL     ERR(8)        O13MSG
024400150123     C                   ENDIF
024500150121     c                   eval      o13roc='S'
024600150123     C                   MOVEL     '5'           O13ERR
024700150123     C                   GOTO      ENDC01
024800150121     c                   endif
024900150121     c                   endif
025000970707     C*
025100970707    3C     I13CNV        IFEQ      'S'
025200970707     C     E13FZV        ANDNE     1
025300150121     c     i13cnv        oreq      'Y'
025400150121     c     w_captll      andeq     *blanks
025500150121     c     e13fzv        andne     1
025600970707     C                   MOVEL     '5'           O13ERR
025700970707     C* ERRORE FORZABILE SE = "S"
025800970707    5C     I13SZV        IFEQ      'S'
025900970708     C                   MOVEL     ERR(5)        O13MSG
026000970707     C                   MOVEL     1             E13FZV
026100970707     C                   ELSE
026200970708     C                   MOVEL     ERR(4)        O13MSG
026300970707    5C                   ENDIF
026400970707     C                   GOTO      ENDC01
026500970707    4C                   ENDIF
026600970707    3C                   ENDIF
026700970707     C**
026800970707     C* PROCEDURA AFFIDABILITA' =1
026900970707     C*  LA 1 VOLTA VADO IN INTERROGAZIONE
027000970707    3C     I13AF1        IFEQ      'S'
027100970707     C     O95LIA        ANDEQ     '1'
027200970707     C*
027300970707    4C     E13FZ1        IFNE      1
027400970707     C* RICHIAMO L'INTERROGAZIONE
027500970707     C                   CLEAR                   DSSI00
027600970707     C                   MOVEL     'L01'         D00OP0
027700970707     C                   MOVEL     I95LOC        D00LOC
027800970707     C                   MOVEL     I95DAT        D00DRI
027900970707     C                   MOVEL     1             E13FZ1
028000150731     c                   clear                   tisi30ds
028100150731     c                   movel     'S'           i30flg
028200150731     c                   eval      I30NAR=i95nar
028300150731     c                   eval      I30CAP=i95cap
028400150731     c                   eval      I30LOC=i95loc
028500150731     c                   eval      I30PRV=i95prv
028600970707     C                   CALL      'TISI30R'
028700970707     C                   PARM                    KPJBA
028800970707     C                   PARM                    DSSI00
028900150731     c                   parm                    tisi30ds
029000970707     C* SE SELEZIONE
029100970707    5C     D00CAP        IFNE      *BLANKS
029200970707     C                   MOVEL     D00CAP        O95CAP
029300970710     C                   MOVEL     'S'           O13ROC
029400970707     C                   MOVEL     D00LOC        O95LOC
029500970710     C                   MOVEL     'S'           O13ROL
029600970707     C                   MOVEL     D00NAR        O95NAR
029700970710     C                   MOVEL     'S'           O13RON
029800970707     C                   MOVEL     D00PRV        O95PRV
029900970710     C                   MOVEL     'S'           O13ROP
030000970707     C                   MOVEL     '3'           O13ERR
030100970707     C                   GOTO      ENDC01
030200970707    5C                   ENDIF
030300970707    4C                   ENDIF
030400970707     C**
030500970707     C* SE C'E' ANCORA LIVELLO AFFIDABILITA' 1 : ERRORE FORZABILE
030600970707    4C     E13FZ2        IFNE      1
030700970707     C                   MOVEL     '3'           O13ERR
030800970707     C*
030900970707    5C     I13SZ2        IFEQ      'S'
031000970708     C                   MOVEL     ERR(7)        O13MSG
031100970707     C                   MOVEL     1             E13FZ2
031200970707     C                   ELSE
031300970708     C                   MOVEL     ERR(6)        O13MSG
031400970707    5C                   ENDIF
031500970707     C                   GOTO      ENDC01
031600970707    4C                   ENDIF
031700970707     C**
031800970707     C* LA PROVINCIA DEVE PERO' ESSERE CONGRUENTE CON IL CAP ALMENO
031900970707    4C     O95NCP        IFEQ      'S'
032000970707     C     I95PRV        ANDNE     O95PRV
032100970707     C     I95PRV        ANDNE     O95PRA
032200970707     C                   MOVEL     ERR(3)        O13MSG
032300970707     C                   MOVEL     '4'           O13ERR
032400970707     C                   GOTO      ENDC01
032500970707    4C                   ENDIF
032600970707    3C                   ENDIF
032700970707     C**
032800970707   X2C                   ELSE
032900970707     C**
033000970707     C                   MOVEL     O95MSG        O13MSG
033100970707     C* ERRORE SU :
033200970707     C                   SELECT
033300970707     C     O95ERR        WHENEQ    'C'
033400970707     C     O95ERR        OREQ      'E'
033500970707     C                   MOVEL     '4'           O13ERR
033600150205     c     o95err        wheneq    'H'
033700150205     c     i13cnv        andeq     'Y'
033800150313     c**   o95err        oreq      'B'
033900150313     c**   i13cnv        andeq     'Y'
034000150123     c                   clear                   o13err
034100150123     c                   exsr      sr_NormTell
034200150123     c                   if        w_captll= *blanks
034300150205     c* attivo la ricerca cappario  se non richiamato da batch
034400150123     c                   exsr      sr_rcapp
034500150123     C                   MOVEL     '3'           O13ERR
034600150205     c                   else
034700150123     c                   movel     w_captll      o95cap
034800150123     C     PARMS         IFge      4
034900150123     C     FLGBAC        ANDEQ     'S'
035000150123     C                   MOVEL     ERR(10)       O13MSG
035100150123     C                   ELSE
035200150123     C                   MOVEL     ERR(8)        O13MSG
035300150123     C                   ENDIF
035400150123     c                   eval      o13roc='S'
035500150123     C                   MOVEL     '5'           O13ERR
035600150123     c                   endif
035700970707     C     O95ERR        WHENEQ    'D'
035800970707     C     O95ERR        OREQ      'B'
035900970707     C     O95ERR        OREQ      'H'
036000970707     C     O95ERR        OREQ      'I'
036100970707     C                   MOVEL     '3'           O13ERR
036200970707     C     O95ERR        WHENEQ    '1'
036300970715     C                   MOVEL     '6'           O13ERR
036400970707     C                   OTHER
036500970707     C                   MOVEL     '5'           O13ERR
036600970707     C                   ENDSL
036700970707    2C                   ENDIF
036800970707    1C                   ENDIF
036900970703     C*
037000941019     C     ENDC01        TAG
037100941019     C* CHIUSURA PGM
037200970707    1C     I13TLA        IFEQ      ' '
037300941019     C                   RETURN
037400941019     C                   ELSE
037500970701     C*
037600970701     C                   CLEAR                   DSSI95
037700970701     C                   MOVEL     'C'           I95TLA
037800970701     C                   CALL      'TISI95R'
037900970701     C                   PARM                    DSSI95
038000970701     C*
038100941019     C                   SETON                                        LR
038200941019    1C                   ENDIF
038300120321     c*-----------------------------------------------------------------------
038400120321     c     PrepaMSG      BEGSR
038500120321     c
038600120321     c                   movel     o97flo        dcppflo
038700120321     c* se non utilizzabile per il network indicato errore
038800120321    1c                   if        o97uti='N'
038900120321     c
039000120322     c* Nelle tre ccasistiche di errore imposto la prima parte del messaggio
039100120321     c*  che è uguale
039200120321    2c                   if        §cppall='S'  or
039300120321     c                             (§cppcas='S' and i95fca='S') or
039400120321     c                             (§cpppas='S' and i95tpo='A')
039500120321     c                   select
039600120321     c                   when      o97liv='C'
039700120321     c                   clear                   messag
039800120321     c                   eval      messag =
039900120322     c                             rtvMsgLang('TIS0712':kLIN:i95cap+i97ntw)
040000120321     C                   MOVEL     messag        O13MSG
040100120321     C****               MOVEL     ERR(11)       O13MSG
040200120321     c**                 eval      %subst(o13msg:32:3)=i97ntw
040300120321     c
040400120321     c                   when      o97liv='N'
040500120321     C***                MOVEL     ERR(14)       O13MSG
040600120321     c****               eval      %subst(o13msg:33:3)=i97ntw
040700120321     c                   clear                   messag
040800120321     c                   eval      messag =
040900120322     c                             rtvMsgLang('TIS0713':kLIN:i95nar+i97ntw)
041000120321     C                   MOVEL     messag        O13MSG
041100120321
041200120321     c                   when      o97liv='W'
041300120321     C***                MOVEL     ERR(17)       O13MSG
041400120321     c***                eval      %subst(o13msg:9:3)=i97ntw
041500120321     c                   clear                   messag
041600120321     c                   eval      messag =
041700120322     c                             rtvMsgLang('TIS0714':kLIN:i97ntw)
041800120321     C                   MOVEL     messag        O13MSG
041900120321     c                   endsl
042000120321     C                   MOVEL     '5'           O13ERR
042100120321    2c                   endif
042200120321     c*
042300120321     c* errore su spedizioni con C/Assegno
042400120321     c                   clear                   messag
042500120321     c                   eval      cont=1
042600120321     c
042700120321    2c                   if        §cppcas='S' and i95fca='S'
042800120321     c
042900120321    3c                   dow       messag=*blanks
043000120321     c
043100120321    4c                   select
043200120614     c                   when      cont=1 or (klin<>'it' and klin<>'  ')
043300120614     c                   if        klin='it' or klin='  '
043400120614     c                   eval      messag ='per spedizioni con Contrassegno'
043500120614     c                   endif
043600120614     c                   if        klin='en'
043700120614     c                   eval      messag ='for cash-on-delivery shipments'
043800120614     c                   endif
043900120614     c                   if        klin='de'
044000120614     c                   eval      messag ='für Nachnahmesendungen'
044100120614     c                   endif
044200120614     c                   if        klin='fr'
044300120614     c                   eval      messag ='pour expéditions en contre remb'
044400120614     c                   endif
044500120614     c
044600120321     c                   when      cont=2
044700120321     c                   eval      messag ='per sped.con C/Assegno'
044800120321    4c                   endsl
044900120321     c
045000120321     c                   exsr      CtrLeng
045100120321    3c                   enddo
045200120321     c
045300120321    2c                   endif
045400120321     c
045500120321    2c                   if        §cpppas='S' and i95tpo='A'
045600120321     c
045700120321    3c                   dow       messag=*blanks
045800120321     c
045900120321    4c                   select
046000120614     c                   when      cont=1 or (klin<>'it' and klin<>'  ')
046100120614     c                   if        klin='it' or klin='  '
046200120321     c                   eval      messag ='per spedizioni in porto Assegnato'
046300120614     c                   endif
046400120614     c                   if        klin='en'
046500120614     c                   eval      messag ='for carriage forward shipments'
046600120614     c                   endif
046700120614     c                   if        klin='de'
046800120614     c                   eval      messag ='für Sendungen als Warennachnahme'
046900120614     c                   endif
047000120614     c                   if        klin='fr'
047100120614     c                   eval      messag ='pour expéditions en port dû'
047200120614     c                   endif
047300120614     c
047400120321     c                   when      cont=2
047500120321     c                   eval      messag ='per spedizioni in Assegnato'
047600120321    4c                   endsl
047700120321     c
047800120321     c                   exsr      CtrLeng
047900120321    3c                   enddo
048000120321     c
048100120321    2c                   endif
048200120321    1c                   endif
048300120321     c
048400120321     c***
048500120321    2c***                if        §cppcas='S' and i95fca='S'
048600120321     c***                select
048700120321     c***                when      o97liv='C'
048800120321     C***                MOVEL     ERR(12)       O13MSG
048900120321     c***                eval      %subst(o13msg:32:3)=i97ntw
049000120321     c***                when      o97liv='N'
049100120321     C***                MOVEL     ERR(15)       O13MSG
049200120321     c***                eval      %subst(o13msg:33:3)=i97ntw
049300120321     c***                when      o97liv='W'
049400120321     C***                MOVEL     ERR(18)       O13MSG
049500120321     c***                eval      %subst(o13msg:9:3)=i97ntw
049600120321     c***                endsl
049700120321     C***                MOVEL     '5'           O13ERR
049800120321    7c***                endif
049900120321    7c***                if        §cpppas='S' and i95tpo='A'
050000120321     c***                select
050100120321     c***                when      o97liv='C'
050200120321     C***                MOVEL     ERR(13)       O13MSG
050300120321     c***                eval      %subst(o13msg:32:3)=i97ntw
050400120321     c***                when      o97liv='N'
050500120321     C***                MOVEL     ERR(16)       O13MSG
050600120321     c***                eval      %subst(o13msg:33:3)=i97ntw
050700120321     c***                when      o97liv='W'
050800120321     C***                MOVEL     ERR(19)       O13MSG
050900120321     c***                eval      %subst(o13msg:9:3)=i97ntw
051000120321     c***                endsl
051100120321     C***                MOVEL     '5'           O13ERR
051200120321    7c***                endif
051300120321    6c***                endif
051400120321    5c***                endif
051500120321     c                   ENDSR
051600120321     c*-----------------------------------------------------------------------
051700120321     c     Ctrleng       BEGSR
051800120321     c                   clear                   messag_uni
051900120321     c                   eval      messag_uni=%trim(o13msg)+' ' +
052000120321     c                             messag
052100120321     c                   eval      leng=%len(%trim(messag_uni))
052200120321     c
052300120322     c                   if        leng>78 and cont<2
052400120321     c                   clear                   messag
052500120321     c                   eval      cont=cont+1
052600120321     c                   else
052700120321     c                   eval      o13msg=messag_uni
052800120321     c                   endif
052900120321     c
053000120321     c                   ENDSR
053100150121     c*-----------------------------------------------------------------------
053200150121     c     sr_NormTell   BEGSR
053300150121     c                   clear                   w_captll
053400150121
053500150206     c                   if        i95ind<>*blanks
053600150206
053700150121     C                   EVAL      pInNazione   = i95nar
053800150121     C                   EVAL      pInProvincia = i95prv
053900150121     C                   EVAL      pInComune    = i95loc
054000150121     C                   EVAL      pInLocalita  = *blanks
054100150121     C                   EVAL      pInCAP       = i95cap
054200150123     C                   EVAL      pInVia       = i95ind
054300150121     C                   EVAL      pInCivico    = *blanks
054400150121     C                   EVAL      pInScore     = *blanks
054500150121     C                   CALL      'UBTL05R02'
054600150121     C                   PARM                    pInNazione
054700150121     C                   PARM                    pInProvincia
054800150121     C                   PARM                    pInComune
054900150121     C                   PARM                    pInLocalita
055000150121     C                   PARM                    pInCAP
055100150121     C                   PARM                    pInVia
055200150121     C                   PARM                    pInCivico
055300150121     C                   PARM                    pInScore
055400150121     C                   PARM                    pOutEsito
055500150121     C                   PARM                    pOutEsitoDes
055600150121     C                   PARM                    pOutTotFound
055700150121     C                   PARM                    pOutResults
055800150121     c                   z-add     1             inx
055900150121     c                   dow       inx<=pOutTotFound
056000150121     c                   if        pOutResults(inx).esatto='0' and
056100150123     c                             pOutResults(inx).LivNorm='V' and
056200150123     c                             pOutResults(inx).Prov=i95prv
056300150121     c                   eval      w_Captll=pOutResults(inx).CAP
056400150121     c                   leave
056500150121     c                   endif
056600150121     c                   add       1             inx
056700150121     c                   enddo
056800150206
056900150206     c                   endif
057000150121     c                   endsr
057100150123     c*-----------------------------------------------------------------------
057200150123     c     sr_rcapp      BEGSR
057300150205     c                   if        flgbac<>'S'
057400150123     C                   CLEAR                   DSSI00
057500150123     C                   MOVEL     'L01'         D00OP0
057600150123     C                   MOVEL     i95loc        D00LOC
057700150123     C                   MOVEL     I95dat        D00DRI
057800150123     C                   MOVEL     i95nar        D00NAR
057900150731     c                   clear                   tisi30ds
058000150731     c                   movel     'S'           i30flg
058100150731     c                   eval      I30NAR=i95nar
058200150731     c                   eval      I30CAP=i95cap
058300150731     c                   eval      I30LOC=i95loc
058400150731     c                   eval      I30PRV=i95prv
058500150123     C                   CALL      'TISI30R'
058600150123     C                   PARM                    KPJBA
058700150123     C                   PARM                    DSSI00
058800150731     c                   parm                    tisi30ds
058900150123     C* SE SELEZIONATO CAP
059000150123     C     D00CAP        IFNE      *BLANKS
059100150123     C                   MOVEL     D00CAP        o95CAP
059200150123     C                   MOVEL     'S'           O13ROC
059300150123     C                   MOVEL     D00LOC        o95LOC
059400150123     C                   MOVEL     'S'           O13ROL
059500150123     C                   MOVEL     D00NAR        o95NAR
059600150123     C                   MOVEL     'S'           O13RON
059700150123     C                   MOVEL     D00PRV        o95PRV
059800150123     C                   MOVEL     'S'           O13ROP
059900150123     C                   END
060000150205     c                   clear                   o13msg
060100150205     c                   endif
060200150123     C**
060300150123     c                   endsr
060400000721**
060500970710C.A.P. incongruente con provincia e/o localita'                                1
060600970707C.A.P. inesistente                                                             2
060700970707C.A.P. incongruente con la provincia                                           3
060800970707Inserito un CAP generico di citta' che ha il Viario                            4
060900970707Inserito un CAP generico di citta' che ha il Viario: premere ENTER per FORZARE 5
061000970707Localita' INESISTENTE per CAP con eccezioni di localita'                       6
061100970707Localita' INESISTENTE per CAP con eccezioni di localita': ENTER per forzare    7
061200970714ATTENZIONE!! Presenti dati incongruenti: modificati.Enter per continuare
061300970710C.A.P. incongruente con provincia e/o localita': ENTER per forzare             9
061400000721ATTENZIONE!! Presenti dati incongruenti: modificati.                          10
061500120319C.A.P. xxxxxxxxx non servito dal network XXx                                  11
061600120321C.A.P. xxxxxxxxx non servito dal network xxx per spedizioni con C/Assegno     12
061700120319C.A.P. xxxxxxxxx non servito dal network xxx per spedizioni in porto Assegnato
061800120321Nazione "xxx" non servita dal network xxx
061900120321
062000120321
062100120321Network xxx non utilizzabile                                                  17
062200080414Network xxx non utilizzabile per spedizioni con C/Assegno                     18
062300080414Network xxx non utilizzabile per spedizioni in porto assegnato                19
