{
      ORM Brasil é um ORM simples e descomplicado para quem utiliza Delphi

                   Copyright (c) 2016, Isaque Pinheiro
                          All rights reserved.

                    GNU Lesser General Public License
                      Versão 3, 29 de junho de 2007

       Copyright (C) 2007 Free Software Foundation, Inc. <http://fsf.org/>
       A todos é permitido copiar e distribuir cópias deste documento de
       licença, mas mudá-lo não é permitido.

       Esta versão da GNU Lesser General Public License incorpora
       os termos e condições da versão 3 da GNU General Public License
       Licença, complementado pelas permissões adicionais listadas no
       arquivo LICENSE na pasta principal.
}

{ @abstract(ORMBr Framework.)
  @created(20 Jul 2016)
  @author(Isaque Pinheiro <isaquepsp@gmail.com>)
  @author(Skype : ispinheiro)

  ORM Brasil é um ORM simples e descomplicado para quem utiliza Delphi.
}

{
  Compiler				CompilerVersion	Defined Symbol	Used BPL
  Delphi 10.2 Tokyo		32 				VER320			25
  Delphi 10.1 Berlin	31				VER310			24
  Delphi 10 Seattle		30				VER300          23
  Delphi XE8			29 				VER290          22
  Delphi XE7			28 				VER280          21
  Delphi XE6			27 				VER270          20
  Delphi XE5			26 				VER260          19
  Delphi XE4			25 				VER250          18 
  Delphi XE3			24 				VER240          17
  Delphi XE2			23 				VER230          16
  Delphi XE	        	22 				VER220          15
  Delphi 2010 	        21 				VER210          14
  Delphi 2009			20 				VER200          13
  Delphi 2007 .NET 		19 				VER190          12
  Delphi 2007 	        18.5    		VER185          12
  Delphi 2006 	        18 				VER180
  Delphi 2005 	        17 				VER170
  Delphi 8 .NET         16 				VER160
  Delphi 7 				15 				VER150
  Delphi 6 				14 				VER140
  Delphi 5 				13 				VER130
  Delphi 4 				12 				VER120
  Delphi 3 				10 				VER100
  Delphi 2 	        	9 				VER90
  Delphi 1 				8 				VER80
}

{.$DEFINE LOAD_DYNAMICALLY}
{.$DEFINE DRIVERRESTFUL}
{$DEFINE USEFDMEMTABLE}
{.$DEFINE USECLIENTDATASET}
{.$DEFINE FMX}

{$IFDEF FMX}
  {$DEFINE HAS_FMX}
{$ELSE}
  {$DEFINE HAS_VCL}
{$ENDIF}

{$IFDEF USEFDMEMTABLE}
  {$DEFINE USEMEMDATASET}
{$ENDIF}

{$IFDEF USECLIENTDATASET}
  {$DEFINE USEMEMDATASET}
{$ENDIF}

// Delphi ???
{$IFDEF VER330}
  {$DEFINE DELPHI14_UP}
  {$DEFINE DELPHI15_UP}
  {$DEFINE DELPHI16_UP}
  {$DEFINE DELPHI17_UP}
  {$DEFINE DELPHI18_UP}
  {$DEFINE DELPHI19_UP}
  {$DEFINE DELPHI20_UP}
  {$DEFINE DELPHI21_UP}
  {$DEFINE DELPHI22_UP}
  {$DEFINE DELPHI23_UP}
  {$DEFINE DELPHI24_UP}
  {$DEFINE DELPHI25_UP}
  {$DEFINE DELPHI26_UP}
{$ENDIF}

// Delphi TOKYO
{$IFDEF VER320}
  {$DEFINE DELPHI14_UP}
  {$DEFINE DELPHI15_UP}
  {$DEFINE DELPHI16_UP}
  {$DEFINE DELPHI17_UP}
  {$DEFINE DELPHI18_UP}
  {$DEFINE DELPHI19_UP}
  {$DEFINE DELPHI20_UP}
  {$DEFINE DELPHI21_UP}
  {$DEFINE DELPHI22_UP}
  {$DEFINE DELPHI23_UP}
  {$DEFINE DELPHI24_UP}
  {$DEFINE DELPHI25_UP}
{$ENDIF}

// Delphi BERLIN
{$IFDEF VER310}
  {$DEFINE DELPHI14_UP}
  {$DEFINE DELPHI15_UP}
  {$DEFINE DELPHI16_UP}
  {$DEFINE DELPHI17_UP}
  {$DEFINE DELPHI18_UP}
  {$DEFINE DELPHI19_UP}
  {$DEFINE DELPHI20_UP}
  {$DEFINE DELPHI21_UP}
  {$DEFINE DELPHI22_UP}
  {$DEFINE DELPHI23_UP}
  {$DEFINE DELPHI24_UP}
{$ENDIF}

// Delphi SEATLLE
{$IFDEF VER300}
  {$DEFINE DELPHI14_UP}
  {$DEFINE DELPHI15_UP}
  {$DEFINE DELPHI16_UP}
  {$DEFINE DELPHI17_UP}
  {$DEFINE DELPHI18_UP}
  {$DEFINE DELPHI19_UP}
  {$DEFINE DELPHI20_UP}
  {$DEFINE DELPHI21_UP}
  {$DEFINE DELPHI22_UP}
  {$DEFINE DELPHI23_UP}
{$ENDIF}

// Delphi XE8
{$IFDEF VER290}
  {$DEFINE DELPHI14_UP}
  {$DEFINE DELPHI15_UP}
  {$DEFINE DELPHI16_UP}
  {$DEFINE DELPHI17_UP}
  {$DEFINE DELPHI18_UP}
  {$DEFINE DELPHI19_UP}
  {$DEFINE DELPHI20_UP}
  {$DEFINE DELPHI21_UP}
  {$DEFINE DELPHI22_UP}
{$ENDIF}

// Delphi XE7
{$IFDEF VER280}
  {$DEFINE DELPHI14_UP}
  {$DEFINE DELPHI15_UP}
  {$DEFINE DELPHI16_UP}
  {$DEFINE DELPHI17_UP}
  {$DEFINE DELPHI18_UP}
  {$DEFINE DELPHI19_UP}
  {$DEFINE DELPHI20_UP}
  {$DEFINE DELPHI21_UP}
{$ENDIF}

// Delphi XE6
{$IFDEF VER270}
  {$DEFINE DELPHI14_UP}
  {$DEFINE DELPHI15_UP}
  {$DEFINE DELPHI16_UP}
  {$DEFINE DELPHI17_UP}
  {$DEFINE DELPHI18_UP}
  {$DEFINE DELPHI19_UP}
  {$DEFINE DELPHI20_UP}
{$ENDIF}

// Delphi XE5
{$IFDEF VER260}
  {$DEFINE DELPHI14_UP}
  {$DEFINE DELPHI15_UP}
  {$DEFINE DELPHI16_UP}
  {$DEFINE DELPHI17_UP}
  {$DEFINE DELPHI18_UP}
  {$DEFINE DELPHI19_UP}
{$ENDIF}

// Delphi XE4
{$IFDEF VER250}
  {$DEFINE DELPHI14_UP}
  {$DEFINE DELPHI15_UP}
  {$DEFINE DELPHI16_UP}
  {$DEFINE DELPHI17_UP}
  {$DEFINE DELPHI18_UP}
{$ENDIF}

// Delphi XE3
{$IFDEF VER240}
  {$DEFINE DELPHI14_UP}
  {$DEFINE DELPHI15_UP}
  {$DEFINE DELPHI16_UP}
  {$DEFINE DELPHI17_UP}
{$ENDIF}

// Delphi XE2
{$IFDEF VER230}
  {$DEFINE DELPHI14_UP}
  {$DEFINE DELPHI15_UP}
  {$DEFINE DELPHI16_UP}
{$ENDIF}

// Delphi XE
{$IFDEF VER220}
  {$DEFINE DELPHI14_UP}
  {$DEFINE DELPHI15_UP}
{$ENDIF}

//Delphi 2010
{$IFDEF VER210}
  {$DEFINE DELPHI14_UP}
{$ENDIF}

{$IFDEF DELPHI15_UP}
  {$DEFINE FORMATSETTINGS}
{$ENDIF}
