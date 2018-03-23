000100100827     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP('BARTVAS')
000200100826     H decedit('0,') datedit(*ymd/) option(*nodebugio)
000300990901     F*------------------------------------------------------------------------*
000400990901     F* DATA BASE
000500990901     F*------------------------------------------------------------------------*
000600100907     FTIABL01L  UF   E           K DISK
000700100921     FTIVSS01L  IF   E           K DISK
000800100826     FPRTF198   O    F  198        PRINTER usropn
000900001005     F*------------------------------------------------------------------------*
001000100826     D*------------------------------------------------------------------------*
001100100827
001200100827     D*-------------------
001300100827     D* DEFINIZIONI PROCEDURE ESTERNE
001400100827     D*-------------------
001500100827     D/COPY GAITRASRC/SRCPROTOPI,UBUTISGI
001600100827     D/COPY GAITRASRC/SRCPROTOPR,UBUTISGI
001700100827
001800100827
001900100826     c
002000100826      *
002100100826     c     *Entry        plist
002200100826     c                   parm                    IN_ELA            1
002300100826      *
002400100826      * Verifico se lancio in aggiornamento
002500100826     c                   if        IN_ELA = 'A'
002600100826     c                   seton                                        55
002700100826     c                   else
002800100826     c                   setoff                                       55
002900100826     c                   endif
003000100826      *
003100100826      * Inizializzazioi contatori d wrk
003200100826     c                   z-add     *zeros        wContaUPD         6 0
003300100826     c                   z-add     *zeros        wContaTOT         6 0
003400100826      *
003500100826      * Inizializzo riga d stampa
003600100826     c                   movel     *blanks       wRigaPrt        150
003700100826      *
003800100826      * Apro il printer file
003900100826     c                   open      PRTF198
004000100826      *
004100100826     c                   if        IN_ELA = 'A'
004200100921     c                   eval      wRigaPrt='TIS7VSS3R - ' +
004300100827     c                                      '* Elaborazione in AGGIORNAMENTO *'
004400100826     c                   else
004500100921     c                   eval      wRigaPrt='TIS7VSS3R - ' +
004600100827     c                                      '* Elaborazione in SIMULAZIONE *'
004700100826     c                   endif
004800100826     c                   except    staLista
004900100921     c                   eval      wRigaPrt = 'Eliminazione orfani di ' +
005000100921     c                                        'abilitazione TT'
005100100827     c                   except    staLista
005200100826     c                   eval      wRigaPrt=''
005300100826     c                   except    staLista
005400100826      *
005500100921     c     *start        setll     tiabl01l
005600100921     c                   read      tiabl01l
005700100921     c                   dow       not %eof(tiabl01l)
005800100827      *
005900100827      * Inizializzazione variabili d wrk
006000100827     c                   add       1             wContaTOT
006100100826      *
006200100826      * Se lancio in aggiornamento => procedo effettivamente alla modifica
006300100921     c                   eval      vssSUN = ablSUN
006400100921     c                   eval      vssISV = 'TT'
006500100921     c     KEY_vss01_C   chain     tivss01l
006600100921     c                   if        not %found(tivss01l)
006700100921      *
006800100921      * Listo utente / cliente corrente
006900100921     c                   eval      wRigaPrt = ' Utente: '   + ablSUN +
007000100921     c                                        ' Cliente: '  + ablUTE
007100100921     c                   except    staLista
007200100921     c                   add       1             wContaUPD
007300100921      *
007400100921     c                   if        *in55
007500100921     c                   exsr      CHKSGI
007600100921     c                   if        pOutEsito = '1'
007700100907     c                   delete    tiabl000
007800100921     c                   endif
007900100907     c                   endif
008000100826     c                   endif
008100100826      *
008200100827      * Proseguo con la lettura sino a fine file
008300100921     c                   read      tiabl01l
008400100826     c                   enddo
008500100826      *
008600100826      * Stampa totali finali
008700100826     c                   eval      wRigaPrt = *blanks
008800100826     c                   except    staLista
008900100921     c                   eval      wRigaPrt = 'TOT ESTENSIONI ELIM: '+
009000100826     c                                         %editc(wContaUPD:'X')
009100100826     c                   except    staLista
009200100921     c                   eval      wRigaPrt = 'TOT ESTENSIONI ELAB: '+
009300100826     c                                         %editc(wContaTOT:'X')
009400100826     c                   except    staLista
009500100826      *
009600100826      * Chiudo il printer file
009700100826     c                   close     PRTF198
009800100826      *
009900100826     c                   SETON                                        LR
010000100826     C*------------------------------------------------------------------------*
010100100827
010200100827
010300100827
010400100827     C*------------------------------------------------------------------------*
010500100827     C* CHKSGI - GESTIONE MODIFICA UTENTI SGI
010600100827     C*------------------------------------------------------------------------*
010700100827     C     CHKSGI        BEGSR
010800100827      /FREE
010900100827               if UBUTISGI_Init = 0;
011000100827               endif;
011100100827      /END-FREE
011200100827     C*
011300100827      /FREE
011400100921               pInSUN = ablSUN;
011500100921               pInKSU = '0'+ablUTE;
011600100827
011700100921               if UBUTISGI_DisableSgiUsr(pInSUN:pInKSU:pOutEsito) = 0;
011800100921               endif;
011900100827
012000100827      /END-FREE
012100100827     C
012200100827     C
012300100827      /FREE
012400100827               if UBUTISGI_Finalize = 0;
012500100827               endif;
012600100827      /END-FREE
012700100827     C*
012800100827     C                   ENDSR
012900100826
013000100826
013100100826
013200990901     C*------------------------------------------------------------------------*
013300990901     C* *INZSR - OPERAZIONI INIZIALI
013400990901     C*------------------------------------------------------------------------*
013500990901     C     *INZSR        BEGSR
013600100921     C*
013700100921     C* Chiave su file TIVSS01L - Completa
013800100921     C     KEY_vss01_C   KLIST
013900100921     C                   KFLD                    vssSUN
014000100921     C                   KFLD                    vssISV
014100001005     C*
014200990901     C                   ENDSR
014300100826     C*------------------------------------------------------------------------*
014400100826     OPRTF198   E            staLista    1
014500100826     O                       wRigaPrt            +1
