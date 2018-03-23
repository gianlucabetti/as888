000100081209     H DECEDIT('0,') DATEDIT(*YMD.)
000200091023     H* TRMZ69R  *---------------------------------------------------*
000300091023     H* x stampare   AITRA con le descrizioni SOCIETA' da PROJ
000400090410     H*        e l'AUTISTA da FIAPD
000500000000     H*--------------------------------------------------------------*
000600091023     Faitra01L  iF   E           K DISK
000700091023     Faitrs01L  iF   E           K DISK
000800090410     Ffiapd01L  iF   E           K DISK
000900090102      *
001000081110     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001100090410     C*****************************************************************
001200091023     D SOC_PROJ        s                   like(traSOC)
001300091023     D AUT_anagra      s                   like(traAUT)
001400091023     D PIVA_APD        s                   like(S4_PARTITAIVA)
001500091023     D trMZ70s1DS    E DS                  prefix(S1_)
001600091023     D trMZ70s4DS    E DS                  prefix(S4_)
001700091023     D kpjba         E DS
001800900521     C*****************************************************************
001900091023     c     *entry        plist
002000091023     c                   parm                    kpjba
002100091023      *
002200081209     C                   SETON                                        LR
002300081110      *
002400090410     c     *loval        setll     aitra01l
002500090410     c                   read      aitra01l
002600090410     c                   dow       not %Eof(aitra01l)
002700091023      *
002800091023      *  solo i NON disaccreditati
002900091026     c                   IF        TRADFI = 0 and traann=' ' and
003000091026     c                             TRAsco = ' ' and
003100091026     c                             (TRAkau > 0 or traBPT > 0)
003200091023      *
003300091023      **  Routine x Reperire dati Fornitore: PROJ
003400090410      **    La routine in base alla sottonatura può funzionare
003500090410      **     x F=Fornitore/C=Cliente
003600091023     c                   clear                   trovato_TRS       1
003700091023     c                   clear                   trovato_PRJ       1
003800091023     c                   clear                   trovato_APD       1
003900091023     c                   clear                   trovata_PIVA      1
004000091023     c                   clear                   SOC_PROJ
004100091023     c                   clear                   AUT_anagra
004200091023     c                   clear                   PIVA_APD
004300091023     c*
004400090410     c     tranrc        chain     aitrs01l
004500091023     c                   IF        NOT %Found(aitrs01l)
004600091023     c                   eval      trovato_TRS ='N'
004700091023     c                   else
004800091023      *  Decodifica PROJ soggetto Ragione Sociale
004900090410     c                   clear                   trmz70s1DS
005000091023     C                   movel(p)  TRsiva        S1_PartitaIVA                     Input
005100091023     C                   movel(p)  'F'           S1_SottoNatur                     Input "F/C"
005200091023     C                   movel(p)  TRsSOCG       S1_Societa                        Input/Output
005300091023     C                   movel(p)  TRsFIL        S1_Unita                          Input/Output
005400090410     c                   call      'TRMZ70SR1'
005500090410     C                   PARM                    trmz70s1DS                     Input
005600091023     c                   if        S1_Errore <> '0'
005700091023     c                   eval      trovato_PRJ ='N'
005800091023     c                   else
005900091023     c                   eval      SOC_PROJ = S1_RAGSOCKSC
006000090410     c                   end
006100090410      *
006200091023      *     TRAKAU  da FIAPD
006300090410     c                   if        traKAU > 0
006400091023      *
006500091023     c                   eval      Tipo    = 'A'
006600091026     C                   eval      TipoAut = 'Cit'
006700091023     c                   eval      Autista = TRAKAU
006800090410     c     kapd          chain     fiapd01l
006900091023     c                   if        NOT %Found(fiapd01l)
007000091023     c                   eval      trovato_APD ='N'
007100091023     c                   else
007200091023     c                   eval      AUT_anagra = apdrsF
007300090410     c                   end
007400091023      **
007500091023      ** se Cod.Aut. è vuoto e c'è quello delle Affl./Defl.
007600091023     c                   elseIF    traKAU=0 and trabPT>0
007700091023      **
007800091023      *     TRABPT  da FIAPD
007900091023     c                   eval      Tipo    = 'D'
008000091026     C                   eval      TipoAut = 'Def'
008100091023     c                   eval      Autista = TRABPT
008200091023     c     kapd          chain     fiapd01l
008300091023     c                   if        NOT %Found(fiapd01l)
008400091023     c                   eval      trovato_APD ='N'
008500091023     c                   else
008600091023     c                   eval      AUT_anagra = apdrsF
008700091023     c                   end
008800091023      *
008900091023     c                   end
009000091023      **
009100091023     c                   endIF
009200090410      *
009300091023      *  Decodifica con PROJ - la PIVA da confrontare
009400091023     c                   clear                   trmz70s4DS
009500091026     C                   movel(p)  'F'           S4_SottoNatur                     Input "F/C"
009600091026     C                   movel(p)  *zeros        S4_KEYKSC                         Input/Output
009700091026     C                   move      apdKSC        S4_KEYKSC                         Input/Output
009800091023     C                   movel(p)  apdCSF        S4_SOCIETA                        Input/Output
009900091023     c                   call      'TRMZ70SR4'
010000091023     C                   PARM                    trmz70s4DS                     Input
010100091023     c                   if        S4_errore <> '0'
010200091023     c                   eval      trovata_PIVA ='N'
010300091023     c                   else
010400091023     c                   eval      PIVA_APD = S4_PARTITAIVA
010500091023     c                   end
010600091023      *
010700091023      *   Stampa se diversa la Rag.Soc fra PROJ e AITRA
010800091023     c                   if        trovato_TRS ='N'
010900091026     c   OF              except    TESTA
011000091023     c                   except    no_TRS
011100091023     c                   else
011200091023      *
011300091023     c                   if        trovato_PRJ ='N'
011400091026     c   OF              except    TESTA
011500091023     c                   except    no_PROJ
011600091023     c                   else
011700091023      *
011800091023     c                   if        trovato_APD ='N'
011900091026     c   OF              except    TESTA
012000091023     c                   except    no_APD
012100091023     c                   else
012200091023      *
012300091023     c                   if        trovata_PIVA ='N'
012400091026     c   OF              except    TESTA
012500091023     c                   except    no_PIV_APD
012600091023     c                   else
012700091023      ***
012800091023     c                   if        TRASOC <> SOC_PROJ or
012900091026     c                             TRAAUT <> AUT_anagra
013000091026     c*********                      or TRSIVA <> PIVA_APD
013100091026     c                   setoff                                       010203
013200091026     c                   clear                   segnale1          3
013300091026     c                   clear                   segnale2          3
013400091026     c                   clear                   segnale3          3
013500091026     c                   clear                   segnale4          3
013600091026     c                   clear                   segnale5          3
013700091026     c                   if        TRAAUT <> AUT_anagra
013800091026     c                   seton                                        01
013900091026     c                   eval         segnale1 = '-->'
014000091026     c                   end
014100091026     c                   if        TRASOC <> SOC_PROJ
014200091026     c                   seton                                        02
014300091026     c                   eval         segnale2 = '-->'
014400091026     c                   eval         segnale4 = '<--'
014500091026     c                   end
014600091026     c                   if        TRSIVA <> PIVA_APD
014700091026     c                   seton                                        03
014800091026     c                   eval         segnale3 = '-->'
014900091026     c                   end
015000091023      ***
015100091026     c   OF              except    TESTA
015200091023     c                   except    Dettagli
015300091023      ***
015400091023     c                   end
015500091023      ***
015600091023     c                   end
015700091023     c                   end
015800091023     c                   end
015900091023     c                   end
016000091023      *
016100091023     c                   ENDIF
016200090410     c                   read      aitra01l
016300090410     c                   enddo
016400090410      *
016500081110     C     FINE          TAG
016600081110     c                   return
016700081209     C*------------------------------------------------------------------------*
016800081209     C* *INZSR - OPERAZIONI INIZIALI
016900081209     C*------------------------------------------------------------------------*
017000081209     C     *INZSR        BEGSR
017100081209     C*
017200090410     C     kapd          kLIST
017300091023     C                   kfld                    Tipo              1
017400091023     C                   kfld                    Autista           7 0
017500081209     C*
017600090410     c                   clear                   esito             1
017700091026     C                   clear                   TipoAut           3
017800090410     c                   except    TESTA
017900081209      *
018000081209     C* COSTRUISCO LA DATA FINALE IN FORMATO AAAAMMGG
018100081209     C                   clear                   UDATE_8           8 0
018200081209     C                   MOVEL     *DATE         UDATE_8
018300081209     C*
018400081209     c                   endSR
018500081211     C*---------------------------------------------------------------*
018600081110     OQSYSPRT   E            TESTA            02
018700090410     O                                            1 ' '
018800091026     O                       KNMUS               20
018900091026     O                                           38 ' '
019000091026     O                                         +  1 'Controllo dati Automezzi'
019100091026     O                                         +  1 'con Dati Anagrafici PROJ'
019200091023     O                                          113 'TRMZ69R'
019300091023     O                       UDATE              127 '  /  /  '
019400091023     O                       PAGE          Z    132
019500091026     O          E            TESTA       2  2
019600091026     O                                           38 ' '
019700091026     O                                         +  2 'Dati Automezzi'
019800091026     O                                         + 16 'Dati Anagrafica'
019900091023      *--
020000090410     O          E            NO_APD      1
020100091023     O                                            1 ' '
020200091023     O                                         +  1 'Non c''è anagr. su FIAPD'
020300091023     O                       Tipo              +  1
020400091023     O                       Autista       z   +  1
020500091026     O          E            NO_APD      1
020600091026     O                                            1 ' '
020700091026     O                                         +  1 '------------------------'
020800091023      *--
020900091023     O          E            NO_PIV_APD  1
021000091023     o                                            1 ' '
021100091023     O                                         +  1 'Non trovata P.IVA anagr.'
021200091023     O                       TrsIVA            +  1
021300091023     O                                         +  1 '/'
021400091023     O                       apdKSC        z   +  1
021500091023     O                       apdCSF            +  1
021600091023     o                                         +  1 'NrCnt.'
021700091023     O                       TrsNRC        z   +  1
021800091023     o                                         +  2 'Aut.'
021900091023     O                       Autista       z   +  1
022000091023     O                       TRaAUT            +  2
022100091026     O          E            NO_PIV_APD  1
022200091026     O                                            1 ' '
022300091026     O                                         +  1 '------------------------'
022400091023      *--
022500090410     O          E            NO_PROJ     1
022600091023     o                                            1 ' '
022700091023     O                                         +  1 'Non c''è anagr. su  PROJ'
022800091023     O                       TRsFIL        z   +  5
022900090410     O                       TRsSOCG           +  2
023000090415     O                       TRsIVA            +  2
023100091026     O          E            NO_PROJ     1
023200091026     O                                            1 ' '
023300091026     O                                         +  1 '------------------------'
023400091023      *--
023500090410     O          E            NO_TRS      1
023600091023     o                                            1 ' '
023700091023     O                                         +  1 'Non c''è il Contratto'
023800091023     O                       TRaFIL        z   +  5
023900090410     O                       TRaSOCG           +  2
024000090410     O                       TRaSOC            +  2
024100090410     O                       TRaAUT            +  2
024200090410     O                       TRaKAU        z   +  2
024300091023     O                       TRaBPT        z   +  2
024400090410     O                       TRaNRC            +  5
024500091026     O          E            NO_TRS      1
024600091026     O                                            1 ' '
024700091026     O                                         +  1 '------------------------'
024800081110      ****
024900091023     O          E            Dettagli    1
025000091023     o                                            1 ' '
025100091026     o                                         +  1 'Contr.'
025200091023     O                       TrsNRC        z   +  1
025300091023     o                                         +  2 'Aut.'
025400091023     O                       Autista       z   +  1
025500091026     o                                         +  0 '/'
025600091026     O                       TipoAut           +  0
025700091026      **
025800091026     O          E            Dettagli    1
025900091026     o                                           32 ' '
026000091026     o                       Segnale1          +  0
026100091026     o                                         +  0 'Nome Aut.'
026200091023     O                       TRaAUT            +  1
026300091026     O                       AUT_anagra        +  5
026400091026     O          E    01      Dettagli    0
026500091026     o                                           32 ' '
026600091026     o                       Segnale1          +  0
026700091026     o                                         +  0 'Nome Aut.'
026800091026     O                       TRaAUT            +  1
026900091026     O                       AUT_anagra        +  5
027000091026      **
027100091023     O          E            Dettagli    1
027200091026     o                                           32 ' '
027300091026     o                       Segnale2          +  0
027400091026     o                                         +  0 '     Soc.'
027500091023     O                       TRaSOC            +  1
027600091026     O                       SOC_PROJ          +  5
027700091026     o                       Segnale4          +  2
027800091026     O          E    02      Dettagli    0
027900091026     o                                           32 ' '
028000091026     o                       Segnale2          +  0
028100091026     o                                         +  0 '     Soc.'
028200091026     O                       TRaSOC            +  1
028300091026     O                       SOC_PROJ          +  5
028400091026     o                       Segnale4          +  2
028500091026     O*******   E            Dettagli    1
028600091026     o*******                                    32 ' '
028700091026     o*******                Segnale3          +  0
028800091026     o*******                                  +  0 'PIVA'
028900091026     O*******                TRsIVA            +  1
029000091026     o*******                                  + 11 'PIVA.Anag.'
029100091026     O*******                PIVA_APD          +  1
029200091026     O*******   E    03      Dettagli    0
029300091026     o*******                                    32 ' '
029400091026     o*******                Segnale3          +  0
029500091026     o*******                                  +  0 'PIVA'
029600091026     O*******                TRsIVA            +  1
029700091026     o*******                                  + 11 'PIVA.Anag.'
029800091026     O*******                PIVA_APD          +  1
029900091026     O          E            Dettagli    0  1
030000091026     o                                           32 ' '
030100091023      ****
