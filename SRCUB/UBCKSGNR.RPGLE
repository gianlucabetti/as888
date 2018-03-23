000100170523     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP(*CALLER)
000200170428
000300170428
000400170428      //********************************************************************************************
000500170428      //
000600170428      // Definizione file.
000700170428      //
000800170428      //********************************************************************************************
000900170523     Ffiart27l  IF   E           K DISK    usropn
001000170523     F                                     extfile(LibFileART27)
001100170523     F                                     extdesc('FILTRAPRD/FIART27L')
001200170523
001300170428
001400170428      //********************************************************************************************
001500170428      //
001600170428      // Definizione interfacce procedure.
001700170428      //
001800170428      //********************************************************************************************
001900170523     D/COPY GAITRASRC/SRCPROTOPR,UBRTVNETA
002000170523     D/COPY GAITRASRC/SRCPROTOPI,UBRTVNETA
002100170523     D/COPY GAITRASRC/SRCPROTOPI,UBCKSGNR
002200170428
002300170428
002400170428      //********************************************************************************************
002500170428      //
002600170428      // Definizione variabili.
002700170428      //
002800170428      //********************************************************************************************
002900170523     D LibFileART27    S             21A   inz
003000170523     D
003100170523
003200091027
003300091027
003400080620      //********************************************************************************************
003500080620      //
003600080620      // Definizione parametri procedura.
003700080620      //
003800080620      //********************************************************************************************
003900080620     C     *ENTRY        PLIST
004000170523     C                   PARM                    iCKSGNFLS
004100170523     C                   PARM                    iCKSGNLNA
004200170523     C                   PARM                    iCKSGNNRS
004300170523     C                   PARM                    iCKSGNNSC
004400170523     C                   PARM                    oCKSGNFOUND
004500170523     C                   PARM                    oCKSGNARTDS
004600170523     C*
004700170428     C* Avvio il monitoring del intero flusso
004800170428     C                   monitor
004900170523     C*
005000170523     C* Inizializo l'esito a "errore" e clear dei dati
005100170523     C                   eval      oCKSGNFOUND = -1
005200170523     C                   clear                   oCKSGNARTDS
005300090810     C*
005400170523     C* Cerco il segnacollo richiesto in input
005500170523     C     KEYart27_C    chain     fiart27l
005600170523     C                   if        %found(fiart27l)
005700170523     C*
005800170523     C* Se trovato valorizzo esito a "trovato"
005900170523     C                   eval      oCKSGNFOUND = 1
006000170523     C                   else
006100170523     C*
006200170523     C* Diversamente ... a "non trovato"
006300170523     C                   eval      oCKSGNFOUND = 0
006400170523     C                   endif
006500170428     C*
006600170428     C* Gestisco eventuale errore
006700170428     C                   on-error
006800170523     C
006900170523     C                   if        %open(FIART27L)
007000170523     C                   close     FIART27L
007100170523     C                   endif
007200170523     C                   eval      oCKSGNFOUND = -1
007300170523     C                   clear                   oCKSGNARTDS
007400170428     C                   dump(A)
007500170428     C*
007600170428     C* Arresto il monitoring
007700170428     C                   endmon
007800090810     C*
007900090810     C                   return
008000170428
008100170428
008200170428
008300170428      /TITLE Inizializzazioni
008400170428     C     *inzSR        BEGSR
008500170428     C*
008600170428     C* Definizione chiavi di lettura
008700170523     C     KEYart27_C    klist
008800170523     C                   kfld                    iCKSGNFLS
008900170523     C                   kfld                    iCKSGNLNA
009000170523     C                   kfld                    iCKSGNNRS
009100170523     C                   kfld                    iCKSGNNSC
009200170523     C*
009300170523     C* Reperisco il sistema AS/400 corrente
009400170523     C                   callp     UBRTVNETA_Rtv(PiOutSysNeta)
009500170523     C                   if        %subst(PiOutSysNeta:1:6) = 'SETRAS'
009600170523     C                   eval      LibFileART27 = 'FILTRA201/FIART27L'
009700170523     C                   else
009800170523     C                   eval      LibFileART27 = 'FILTRAPRD/FIART27L'
009900170523     C                   endif
010000170523     C*
010100170523     C* Apertura file "overraidati"
010200170523     C                   if        not %open(FIART27L)
010300170523     C                   open      FIART27L
010400170523     C                   endif
010500170428     C*
010600170428     C                   ENDSR
010700100330     C*--------------------------------------------------------------------
010800100330
