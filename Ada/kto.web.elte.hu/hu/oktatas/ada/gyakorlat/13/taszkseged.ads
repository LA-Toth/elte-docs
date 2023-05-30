with Ada.Numerics.Float_Random;
package Taszkseged is

  task type Szemafor( Max: Positive ) is
      entry P;
      entry V;
  end Szemafor;

  protected Veletlen is
     procedure Reset;
     entry General( F: out Float );
  private
     G: Ada.Numerics.Float_Random.Generator;
     Inicializalt: Boolean := False;
  end Veletlen;

end Taszkseged;
