000100081209     H DECEDIT('0,') DATEDIT(*YMD.)
000200091028     H* TRMZ68R  *---------------------------------------------------*
000300091028      *          AGGIORNA AITRA la rag.sociale (TRASOC)
000400000000     H*--------------------------------------------------------------*
000500091028     Ftrmz68d   CF   E             WORKSTN
000600091028      *
000700091028     Faitra01L  uF   E           K DISK
000800091028      *
000900091023     Faitrs01L  iF   E           K DISK
001000090410     Ffiapd01L  iF   E           K DISK
001100090102      *
001200081110     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001300090410     C*****************************************************************
001400091028     D kpjba         E DS
001500091028     C*****************************************************************
001600091023     D SOC_PROJ        s                   like(traSOC)
001700091023     D trMZ70s1DS    E DS                  prefix(S1_)
001800900521     C*****************************************************************
001900091023     c     *entry        plist
002000091023     c                   parm                    kpjba
002100091028     C                   SETON                                        LR
002200091023      *
002300091028     c     inizio        tag
002400091028     c                   clear                   v1cont
002500091028      *
002600091028     c     video         tag
002700091028     c                   setoff                                       99
002800091028      *                 ------------------
002900091028     c                   exfmt     video1
003000091028      *                 ------------------
003100091028     c   KC              goto      fine
003200081110      *
003300091028     c                   exsr      contr1
003400091028     c  nKF
003500091028     cor 99              goto      video
003600091028      *
003700091028     c     finestra      tag
003800091028      *                 ------------------
003900091028     c                   exfmt     videoW
004000091028      *                 ------------------
004100091028     c  nKF
004200091028     cannKC              goto      finestra
004300091028     c  nKF
004400091028     cor KC              goto      video
004500091028      *
004600091028     c                   exsr      esegui
004700091028      *
004800091028     C     FINE          TAG
004900091028     c                   return
005000091028     C*****************************************************************
005100091028      *  Controlli su nr.contratto o codice aut
005200091028     C*****************************************************************
005300091028     c     contr1        begsr
005400091028      *
005500091028     c                   setoff                                       414243
005600091028      *
005700091028      *  numero contratto
005800091028     c                   if        v1cont = 0
005900091028     c                   seton                                        4199
006000091028     c                   else
006100091028      *
006200091028     c     v1cont        setll     aitra01l
006300091028     c                   if        not %equal(aitra01l)
006400091028     c                   seton                                        4199
006500091028     c                   else
006600091028      *
006700091028     c     v1cont        setll     aitrs01l
006800091028     c                   if        not %equal(aitrs01l)
006900091028     c                   seton                                        4199
007000091028     c                   end
007100091028      *
007200091028     c                   endIf
007300091028     c                   end
007400091028      *
007500091028     c                   endsr
007600091028     C*****************************************************************
007700091028      *    Esegue l'aggiornamento
007800091028     C*****************************************************************
007900091028     c     esegui        begsr
008000091028      *
008100091028      *   Aggiorna il nome della società dell'AITRA
008200091023      *
008300091023      **  Routine x Reperire dati Fornitore: PROJ
008400090410      **    La routine in base alla sottonatura può funzionare
008500090410      **     x F=Fornitore/C=Cliente
008600091023     c                   clear                   trovato_TRS       1
008700091023     c                   clear                   trovato_PRJ       1
008800091023     c                   clear                   SOC_PROJ
008900091028      *
009000091028     c     v1Cont        chain     aitrs01l
009100091028      *
009200091023     c                   IF        NOT %Found(aitrs01l)
009300091023     c                   eval      trovato_TRS ='N'
009400091023     c                   else
009500091028      *
009600091023      *  Decodifica PROJ soggetto Ragione Sociale
009700090410     c                   clear                   trmz70s1DS
009800091023     C                   movel(p)  TRsiva        S1_PartitaIVA                     Input
009900091023     C                   movel(p)  'F'           S1_SottoNatur                     Input "F/C"
010000091023     C                   movel(p)  TRsSOCG       S1_Societa                        Input/Output
010100091023     C                   movel(p)  TRsFIL        S1_Unita                          Input/Output
010200090410     c                   call      'TRMZ70SR1'
010300090410     C                   PARM                    trmz70s1DS                     Input
010400091028      *
010500091023     c                   if        S1_Errore <> '0'
010600091023     c                   eval      trovato_PRJ ='N'
010700091023     c                   else
010800091028     c*
010900091023     c                   eval      SOC_PROJ = S1_RAGSOCKSC
011000091028     c*
011100091028     c* aggiorna tutti i records
011200091028     c     v1Cont        setll     aitra01l
011300091028     c     v1Cont        reade     aitra01l
011400091028      *
011500091028     c                   DoW       not %Eof(aitra01l)
011600091028      *
011700091028      *   solo i NON disaccreditati
011800091028     c                   if        tradfi = 0
011900091028     c   OF              except    TESTA
012000091028     c                   except    Dettagli
012100091028     c                   eval      traSOC = SOC_PROJ
012200091028     c                   end
012300091028      ***
012400091028     c                   update    aitra000
012500091028     c     v1Cont        reade     aitra01l
012600091028     c                   endDO
012700091028      *
012800090410     c                   end
012900091023     c                   endIF
013000091023      *
013100091028     c                   ENDsr
013200081209     C*------------------------------------------------------------------------*
013300081209     C* *INZSR - OPERAZIONI INIZIALI
013400081209     C*------------------------------------------------------------------------*
013500081209     C     *INZSR        BEGSR
013600081209      *
013700081209     C* COSTRUISCO LA DATA FINALE IN FORMATO AAAAMMGG
013800081209     C                   clear                   UDATE_8           8 0
013900081209     C                   MOVEL     *DATE         UDATE_8
014000081209     C*
014100091028     c                   except    TESTA
014200091028     C*
014300081209     c                   endSR
014400081211     C*---------------------------------------------------------------*
014500081110     OQSYSPRT   E            TESTA            02
014600090410     O                                            1 ' '
014700091026     O                       KNMUS               20
014800091026     O                                           38 ' '
014900091028     O                                         +  1 'Aggiorna nome Società au'
015000091028     O                                         +  0 'tista da Anagrafica PROJ'
015100091028     O                                          113 'TRMZ68R'
015200091023     O                       UDATE              127 '  /  /  '
015300091023     O                       PAGE          Z    132
015400091028      *-----
015500091026     O          E            TESTA       2  2
015600091028     O                                           68 ' '
015700091026     O                                         +  2 'Dati Automezzi'
015800091028     O                                         + 21 'Dati Anagrafica'
015900091028      *-----
016000091023     O          E            Dettagli    1
016100091023     o                                            1 ' '
016200091028     o                                         +  1 'Nr.Contr.'
016300091028     O                       TraNRC        z   +  1
016400091028      *-----
016500091023     O          E            Dettagli    1
016600091028     o                                           25 ' '
016700091028     o                                         +  2 'Aut.'
016800091028     O                       traKAU        z   +  1
016900091028     o                                         +  1 '/'
017000091028     O                       traBPT        z   +  1
017100091028     o                                         + 10 ' Nome Soc.'
017200091023     O                       TRaSOC            +  1
017300091026     O                       SOC_PROJ          +  5
017400091028      *-----
017500091028     O          E            Dettagli    0
017600091028     o                                           25 ' '
017700091028     o                                         +  2 'Aut.'
017800091028     O                       traKAU        z   +  1
017900091028     o                                         +  1 '/'
018000091028     O                       traBPT        z   +  1
018100091028     o                                         + 10 ' Nome Soc.'
018200091026     O                       TRaSOC            +  1
018300091026     O                       SOC_PROJ          +  5
018400091028      *-----
018500091026     O          E            Dettagli    0  1
018600091026     o                                           32 ' '
