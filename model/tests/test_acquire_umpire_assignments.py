import pathlib,sys,unittest
sys.path.insert(0,str(pathlib.Path(__file__).resolve().parents[1]))
from acquire_umpire_assignments import extract
class TestOfficials(unittest.TestCase):
 def test_single_home_plate(self):
  d=extract({'officials':[{'officialType':'Home Plate','official':{'id':1,'fullName':'A'}}]})
  self.assertEqual((d['status'],d['umpire_id']),('unique_game_assignment',1))
 def test_missing_not_imputed_to_wrong_official(self):
  d=extract({'officials':[{'officialType':'First Base','official':{'id':1,'fullName':'A'}}]})
  self.assertEqual(d['status'],'missing_home_plate');self.assertEqual(d['umpire_id'],'')
 def test_multiple_not_silently_selected(self):
  d=extract({'officials':[{'officialType':'Home Plate','official':{'id':i,'fullName':str(i)}} for i in (1,2)]})
  self.assertEqual(d['status'],'multiple_home_plate');self.assertEqual(d['home_plate_records'],2)
 def test_incomplete_identity_rejected(self):
  with self.assertRaises(ValueError):extract({'officials':[{'officialType':'Home Plate','official':{'fullName':'A'}}]})
if __name__=='__main__':unittest.main()
