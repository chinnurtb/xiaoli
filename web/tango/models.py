
from flask_sqlalchemy import SQLAlchemy

db = SQLAlchemy()

class Setting(db.Model):
    __tablename__ = 'settings'
    id = db.Column(db.Integer, primary_key=True)
    name = db.Column(db.String(80))
    value = db.Column(db.Text())

    def __init__(self, name, value):
        self.name = name
        self.value = value

class Profile(db.Model):
    __tablename__ = 'profiles'
    id = db.Column(db.Integer, primary_key=True)
    key = db.Column(db.String(80))
    value = db.Column(db.Text())

    def __init__(self, key, value):
        self.key = key
        self.value = value

    @classmethod
    def find(key, profiles):
        for p in profiles:
            if key == p.key:
                return p.value
        return None 

