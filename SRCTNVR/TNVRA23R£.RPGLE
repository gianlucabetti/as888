000100081209     H DECEDIT('0,') DATEDIT(*YMD.)
000200090410     H* TNVR16AR *---------------------------------------------------*
000300081110     H*                                                              *
000400090410     H* x aggiornare AITRA con le descrizioni SOCIETA' da PROJ
000500090410     H*        e l'AUTISTA da FIAPD
000600081209     H*                                                              *
000700000000     H*--------------------------------------------------------------*
000800090410     Faitra01L  uF   E           K DISK
000900090410     Faitrs01L  uF   E           K DISK
001000090410     Ffiapd01L  iF   E           K DISK
001100090102      *
001200081110     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001300090410     C*****************************************************************
001400090410     D trMZ70s1DS    E DS
001500900521     C*****************************************************************
001600081209     C                   SETON                                        LR
001700081110      *
001800090410     c     *loval        setll     aitra01l
001900090410     c                   read      aitra01l
002000090410      *
002100090410     c                   dow       not %Eof(aitra01l)
002200090410      *     TRASOC  da PROJ
002300090410      **  Routine x Reperire dati Fornitore:
002400090410      **    La routine in base alla sottonatura può funzionare
002500090410      **     x F=Fornitore/C=Cliente
002600090410     c     tranrc        chain     aitrs01l
002700090410     c                   if        %Found(aitrs01l)
002800090410     c                   clear                   trmz70s1DS
002900090410     C                   movel(p)  TRsiva        PartitaIVA                     Input
003000090410     C                   movel(p)  'F'           SottoNatur                     Input "F/C"
003100090410     C                   movel(p)  TRsSOCG       Societa                        Input/Output
003200090415     C                   movel(p)  TRsFIL        Unita                          Input/Output
003300090410     c                   call      'TRMZ70SR1'
003400090410     C                   PARM                    trmz70s1DS                     Input
003500090410     c                   if        Errore = '0'
003600090410     c                   eval      traSOC = RAGSOCKSC
003700090410     c                   else
003800090410     c                   except    no_PROJ
003900090410     c                   end
004000090410     c*
004100090410      *
004200090410      *     TRAAUT  da FIAPD
004300090410     c                   if        traKAU > 0
004400090410     c                   eval      apdtip = 'A'
004500090410     c                   eval      apdpdr = TRAKAU
004600090410     c     kapd          chain     fiapd01l
004700090410     c                   if        %Found(fiapd01l)
004800090410     c                   eval      traAUT = apdrsF
004900090410     c                   else
005000090410     c                   except    no_APD
005100090410     c                   end
005200090410     c                   end
005300090410      *
005400090410     c                   else
005500090410     c                   except    no_TRS
005600090410     c                   end
005700090410      *
005800090410      *
005900090410     c                   clear                   traiva
006000090410     c                   update    aitra000
006100090410      *
006200090410     c                   read      aitra01l
006300090410     c                   enddo
006400090410      *
006500081110     C     FINE          TAG
006600081110     c                   return
006700081209     C*------------------------------------------------------------------------*
006800081209     C* *INZSR - OPERAZIONI INIZIALI
006900081209     C*------------------------------------------------------------------------*
007000081209     C     *INZSR        BEGSR
007100081209     C*
007200090410     C     kapd          kLIST
007300090410     C                   kfld                    apdtip
007400090410     C                   kfld                    apdpdr
007500081209     C*
007600090410     c                   clear                   esito             1
007700090410     c                   except    TESTA
007800081209      *
007900081209     C* COSTRUISCO LA DATA FINALE IN FORMATO AAAAMMGG
008000081209     C                   clear                   UDATE_8           8 0
008100081209     C                   MOVEL     *DATE         UDATE_8
008200081209     C*
008300081209     c                   endSR
008400081211     C*---------------------------------------------------------------*
008500081110     OQSYSPRT   E            TESTA            02
008600090410     O                                            1 ' '
008700090410     O                                         +  1 'Controllo trascodifica'
008800090410     O                                         +  2 'AITRA x ragioni sociali'
008900090410     O          E            NO_APD      1
009000090410     O                       TRAKAU        z      8
009100090410     O                                         +  2 'No anagr,FIAPD'
009200090410     O          E            NO_PROJ     1
009300090415     O                       TRsFIL        z     30
009400090410     O                       TRsSOCG           +  2
009500090415     O                       TRsIVA            +  2
009600090410     O                                         +  2 'No anagr.ANAGRAFICA PROJ'
009700090410     O          E            NO_TRS      1
009800090410     O                       TRaFIL        z     20
009900090410     O                       TRaSOCG           +  2
010000090410     O                       TRaSOC            +  2
010100090410     O                       TRaAUT            +  2
010200090410     O                       TRaKAU        z   +  2
010300090410     O                       TRaNRC            +  5
010400090410     O                                         +  2 'Non trovato AITRS'
010500081110      ****
