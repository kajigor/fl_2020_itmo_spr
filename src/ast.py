class Body:
    def __init__(self, atoms=[]):
        self.atoms = atoms[::-1]
    def __str__(self):
        st = ""
        st += "Body [atoms = ["
        counter = 1
        for atom in self.atoms:
            st += str(atom)
            if counter < len(self.atoms):
               st += ", "
               counter += 1
        st += "]"
        return st

class P:

  def __init__(self, target, relations=[]):
      self.relations = relations[::-1]
      self.target = target
  def __str__(self):
      st = ""
      st += "P:\n\n"
      st += "Relations:\n\n"
      counter = 1
      for r in self.relations:
         st += str(r)
         st += "\n\n"
      st += str(self.target)
      return st
 

class Relation:

   def __init__(self, atom, body=Body()):
      self.head = atom
      self.body = body

   def __str__(self):
      st = ""
      st += "Relation ["
      st += "head = "
      st += str(self.head)
      st += ", body = "
      st += str(self.body)
      st += "]"
      return st

   
class Target:
   def __init__(self, atoms):
        self.atoms = atoms[::-1]
   def __str__(self):
       st = ""
       st += "Target [atoms = ["
       counter = 1
       for atom in self.atoms:
            st += str(atom)
            if counter < len(self.atoms):
               st += ", " 
               counter += 1
           
       st += "]"
       return st


class Atom:
    def __init__(self, ident, args=[]):
        self.ident = ident
        self.args = args[::-1]
    def __str__(self): 
        st = ""
        st += "Atom [ident = " + str(self.ident) + ", args = "
        st += "["
        counter = 1
        for arg in self.args:
           st += str(arg)
           if counter < len(self.args):
              st += ", "
              counter += 1
        st += "]"
        return st

        
class Arg: 
    def __init__(self, ident):
        self.ident = ident
    def __str__(self):
        if isinstance(self.ident, Atom):
           return str(self.ident)
        return "Var " + str(self.ident)

